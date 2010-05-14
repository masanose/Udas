;+
;Function: stringIsNum
;
;Purpose: Determines if a given string is a number or not.
;
;Inputs: str : the string to be checked
;
;Output: Returns 1 if yes, or if no
;
;
;EXAMPLES:
; if stringIsNum(s) then print, strtrim(double(s),2)
; 
;  % Compiled module: STRINGISNUM.
;THEMIS> print,stringisnum('hello')
;       0
;THEMIS> print,stringisnum('1.234')
;       1
;THEMIS> print,stringisnum('1.234e4')
;       1
;THEMIS> print,stringisnum('1B')
;       1
;THEMIS> print,stringisnum(' 1 ')
;       1
;THEMIS> print,stringisnum(' 1Hello ')
;       0
;THEMIS> print,stringisnum(' 1D ')
;       1
;THEMIS> print,stringisnum(' Hello ')
;       0
;       
;  Notes: String must be only a numerical string and leading or trailing whitespace
;         if true is to be returned.  Strings that include other non-numerical characters
;         will return false even if they have numerical sub-strings.
;
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2008-11-18 09:39:45 -0800 (Tue, 18 Nov 2008) $
;$LastChangedRevision: 3994 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/stringisnum.pro $
;-
function stringIsNum,str

  s = strtrim(str,2)
 
  if $ 
              stregex(s,'^[[:digit:]]+[Bb]$') ne -1 || $  ; EX:  1B
              stregex(s,'^[[:digit:]]+[Uu]*([Ss]|[Ll]{1,2})$') ne -1 || $ ;EX: 1US,1L,1LL,1ULL   
              stregex(s,'^[[:digit:]]+[Uu]$') ne -1 || $ ;EX: 1u
              stregex(s,'^\.+[[:digit:]]+[eEdD][+-]+[[:digit:]]+$') ne -1  ||$ ; EX: .1e+5, .1e-5
              stregex(s,'^\.+[[:digit:]]+[eEdD][[:digit:]]*$') ne -1  ||$ ; EX: .1d,.1e5 
              stregex(s,'^\.+[[:digit:]]+$') ne -1 || $  ; EX: .12342123
              stregex(s,'^[[:digit:]]+\.*[[:digit:]]*[eEdD][+-]+[[:digit:]]+$') ne -1  ||$ ; EX: 1.1e+5, 1.1e-5
              stregex(s,'^[[:digit:]]+\.*[[:digit:]]*[eEdD][[:digit:]]*$') ne -1  ||$ ; EX: 1.1d,1.1e5, 1e5 
              stregex(s,'^[[:digit:]]+\.*[[:digit:]]*$') ne -1 $ ; EX: 1.1,1. 
  then return,1 else return,0

end 