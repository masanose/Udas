; + 
;FUNCTION array_concat
;PURPOSE:
;  Performs array concatenation in a way that handles an empty list.
;  Simple code that gets duplicated everywhere.
;
;Inputs:
;  arg: The argument to be concatenated
;  array: The array to which it should be concatenated, or nothing
;  
;Output:
;  array + arg
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2010-02-09 12:55:58 -0800 (Tue, 09 Feb 2010) $
;$LastChangedRevision: 7237 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/misc/array_concat.pro $
;
; -

function array_concat,arg,array

  compile_opt idl2
  
  if ~is_array(array) && ~keyword_set(array) then begin
    return,[arg]
  endif else begin
    return,[array,arg]
  endelse

end