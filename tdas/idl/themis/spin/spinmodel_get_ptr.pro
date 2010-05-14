;+
; NAME:
;    SPINMODEL_GET_PTR.PRO
;
; PURPOSE:  
;   Returns a pointer to the spin model, specified by a single
;   letter probe designation ('a' through 'f').  This is to avoid
;   having to define the spinmodel common block all over the place.
;
; CATEGORY: 
;   TDAS
;
; CALLING SEQUENCE:
;   model_ptr=spinmodel_get_ptr('a')
;
;  INPUTS:
;    probe: A scalar character, one of 'a' through 'f', specifying which model
;       pointer to return.
;
;  OUTPUTS:
;    model_ptr: The return value is a pointer to the specified spin model,
;       suitable for passing to the other spinmodel manipulation routines.
;
;  KEYWORDS:
;    None.
;    
;  PROCEDURE:
;    Test probe argument for validity; return appropriate value from
;    the spinmodel_common block.
;
;  EXAMPLE:
;     model_ptr=spinmodel_get_ptr('a')
;     spinmodel_test,model_ptr
;
;Written by: Jim Lewis (jwl@ssl.berkeley.edu)
;Change Date: 2007-10-08
;-

function spinmodel_get_ptr,probe
common spinmodel_common,tha_sm_ptr,thb_sm_ptr,thc_sm_ptr,thd_sm_ptr,$
   the_sm_ptr,thf_sm_ptr

ptr = ptr_new()

case probe of
'a': if (ptr_valid(tha_sm_ptr) EQ 1) then ptr=tha_sm_ptr
'b': if (ptr_valid(thb_sm_ptr) EQ 1) then ptr=thb_sm_ptr
'c': if (ptr_valid(thc_sm_ptr) EQ 1) then ptr=thc_sm_ptr
'd': if (ptr_valid(thd_sm_ptr) EQ 1) then ptr=thd_sm_ptr
'e': if (ptr_valid(the_sm_ptr) EQ 1) then ptr=the_sm_ptr
'f': if (ptr_valid(thf_sm_ptr) EQ 1) then ptr=thf_sm_ptr
else: message,'Unrecognized probe identifier: expecting scalar character a,b,c,d,e, or f.'
endcase

if (ptr_valid(ptr) NE 1) then begin
  message,'Warning: returning null spinmodel pointer.  Try loading some data.',/CONTINUE
endif

return, ptr
end
