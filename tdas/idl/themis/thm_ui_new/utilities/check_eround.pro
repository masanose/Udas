
;helper function for formatannotation.pro
;checks if rounding will add a digit to exponential format
pro check_eround, val, neg, dec, precision, exponent

    compile_opt idl2, hidden

  ;get string of digit to be rounded
  v0 = strtrim(string(val, format='(D255.16)'),1)
  v = strmid(v0, 0, 2+precision+1)
  v1 = strmid(v, strlen(v)-1, 1)
  
  ;check if decmal was caught
  if ~is_numeric(v1) then begin
    v = strmid(v0, 0, neg+1+precision+2)
    v1 = strmid(v, strlen(v)-1, 1)
  endif

  ;add length and shift if rounding increases order of magnitude
  if is_numeric(v1) then begin
    if double(v1) ge 5 then begin
      i = neg ? -1:1
      if abs(val +i*10d^(-precision)) ge 10 then begin
        exponent++
        val=val * 10d^(-1)
      endif
    endif
  endif
  
end
