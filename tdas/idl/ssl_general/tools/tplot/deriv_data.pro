;+
;PROCEDURE: deriv_data, n1,n2
;PURPOSE:
;   Creates a tplot variable that is the derivative of a tplot variable.
;INPUT: n1  tplot variable names (strings)
;
;Keywords:
; newname: the name of the tplot variable in which output should be
;    stored. This will produce an error if you use this option with globbing.
; 
; nsmooth: If this keyword is set smoothing will be performed.  The
; number you set this keyword equal to is the width of the smoothing 
; to be applied to the data.  It is the same as the
; width argument to the idl smooth procedure. To get an explanation of
;how this keyword works please see the idl documentation for the
;'width' keyword to the idl 'smooth' procedure.
;
;
; suffix: the suffix to be applied to the input data.  Use this if you
; want to call this procedure on multiple tplot variables
;simultaneously.
;
; replace: set this keyword if you want to replace the original
;variables with the new values
;
; Examples:
;      deriv_data,'thb_fgs_dsl'
;      deriv_data,'th?_fgs_dsl',suffix='_fgsderiv'
;      deriv_data,'thb_fgs_dsl thb_state_pos',nsmth=2
;      deriv_data,'thb_fgs_dsl',newname='fgs_derivd'
;      deriv_data,'the_*',/replace
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2008-04-28 13:27:07 -0700 (Mon, 28 Apr 2008) $
; $LastChangedRevision: 2851 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/branches/tdas_4_00/idl/themis/common/thm_config.pro $
;
;-
PRO deriv_data,names,newname=newname,nsmooth=nsmth,suffix=suffix,replace=replace

ns = tnames(names,n)

;validate keywords and set defaults
if n_elements(ns) gt 1 && keyword_set(newname) then begin
  message,'Cannot call deriv_data on multiple tplot variables and use the newname keyword at the same time'
endif

if keyword_set(replace) && keyword_set(suffix) then begin
  message,'Replace and suffix are mutually exclusive keywords'
endif 

if keyword_set(replace) then begin
   suffix = ''
endif
 
if ~keyword_set(suffix) then begin
  suffix = '_ddt'
endif

for i=0,n-1 do begin
  n1 = ns[i]
  get_data,n1,data=d,dlimits=dl
  if not keyword_set(d)  then begin
     message,/info,'data not defined!'
     continue
  endif

  if ~keyword_set(newname) then begin
     nout = n1+suffix
  endif else begin
     nout = newname
  endelse

  if keyword_set(nsmth) then begin
     d.x = smooth(d.x,nsmth < (n_elements(d.x)-1),/nan,/edge)
     for j=0, n_elements(d.y[0,*])-1 do begin
       d.y[*,j] = smooth(d.y[*,j],nsmth < (n_elements(d.y[*,j])-1),/nan,/edge)
     endfor
  endif

  ;for state derived quantities, guarantee that the metadata properly reflects derivative number  
  str_element,dl,'data_att.st_type',st_type,success=s
  
  if s then begin
    if st_type eq 'pos' then begin
      str_element,dl,'data_att.st_type','vel',/add
    endif else if st_type eq 'vel' then begin
      str_element,dl,'data_att.st_type','acc',/add
    endif
  endif

  if ndimen(d.y) eq 1 then d.y = deriv(d.x,d.y)
  if ndimen(d.y) eq 2 then $
     for j=0,dimen2(d.y)-1 do d.y(*,j) = deriv(d.x,d.y(*,j))

  store_data,nout,data=d,dlimits=dl
endfor
return
end
