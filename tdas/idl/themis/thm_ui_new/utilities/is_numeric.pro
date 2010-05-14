;+
;
;Name: is_num
;
;Purpose: determines if input string is a validly formatted number.  Does
;
;Inputs: s:  the string to be checked
;
;Outputs: 1: if it is validly formatted
;         0: if it is not
;         
;Notes:  Does not consider numbers in complex notation or numbers with trailing type codes to be valid.
;
;Examples:
;   print,is_numeric('1')
;   1
;   print,is_numeric('1.23e45')
;   1
;   print,is_numeric('1.2c34')
;   0
;   print,is_numeric('1B')
;   0
;   print,is_numeric('-1.23d-3')
;   1
;   print,is_numeric('5e+4')
;   1
;   print,is_numeric('5.e2')
;   1
;   print,is_numeric('5.e3.2')
;   0
;   
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-01-09 16:58:39 -0800 (Fri, 09 Jan 2009) $
; $LastChangedRevision: 4402 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/is_numeric.pro $
;-

function is_numeric,s
  return,stregex(strtrim(s,2),'^[-+]?(([0-9]+\.?[0-9]*)|([0-9]*\.?[0-9]+))([EeDd][-+]?[0-9]+)?$') eq 0
end
