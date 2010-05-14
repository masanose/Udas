;+ 
;NAME: 
; thm_ui_getset
;
;PURPOSE:  
;  Superclass to automatically provide common gui methods: "GetAll","SetAll","GetProperty","SetProperty"
;
;
;METHODS:
;  GetProperty
;  SetProperty
;  GetAll
;  SetAll
;
;
;HISTORY:
;
;NOTES:
;  This object differs from other gui objects with respect to its getProperty,setProperty,getAll,setAll methods.  These methods are now provided dynamically
;  so you only need to modify the class definition and the init method to if you want to add or remove a property from the object. 
;  
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 16:51:22 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6725 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/objects/thm_ui_getset__define.pro $
;-----------------------------------------------------------------------------------

;Needed for fun with the get_property/set_property methods
;To test a lower maintainence design for get_property/set_property
function thm_ui_getset::GetAll

   str = create_struct(name=obj_class(self))
   struct_assign,self,str

   return, str

end

pro thm_ui_getset::SetAll, str

   struct_assign,str,self

end

;check whether the values are equal within a relative tolerance
;used to check whether a value was modified
function thm_ui_getset::equalTolerance,val1,val2

  tolerance = 1d-12
  
  if val1 eq val2 || (~finite(val1) && ~finite(val2)) then return,1
  
  if val1 eq 0 then begin
    return, val2 lt tolerance && val2 gt -tolerance
  endif else begin
    return, ((val2/val1)-1) lt tolerance && ((val2/val1)-1) gt -tolerance
  endelse
  
end

;determines whether touched variables were modified during the last iteration
pro thm_ui_getset::setTouched

  self_struct = self->getAll()
  self_tg = strlowcase(tag_names(self_struct))
  
  for i = 0,n_elements(self_tg) - 1 do begin
    if n_elements(self_struct.(i)) eq 1 && obj_valid(self_struct.(i)) then begin
      if obj_isa(self_struct.(i),'IDL_Container') then begin
        for j = 0,(self_struct.(i))->count()-1 do begin
          ((self_struct.(i))->get(position=j))->setTouched
        endfor
      endif else begin
        (self_struct.(i))->setTouched
      endelse
    endif 
  endfor
  
  origidx = where(self_tg eq 'origsettings',c)
  
  if c ne 1 || ~ptr_valid(self_struct.(origidx)) then return
  
  touchedIdx = where(stregex(self_tg,'^touched',/boolean),c)
  
  if c eq 0 then return
  
  for i = 0,n_elements(touchedIdx)-1 do begin
  
    tag = self_tg[touchedIdx[i]]
    
    field = strmid(tag,7,strlen(tag)-7)
    
    fieldidx = where(self_tg eq field,c)
    
    if c ne 1 then continue
    
    if is_num(self_struct.(fieldidx),/floating) then begin
      if ~self->equalTolerance(self_struct.(fieldidx),(*self_struct.(origidx)).(fieldidx)) then begin
        self_struct.(touchedIdx[i]) = 1
      endif
    endif else begin
      if self_struct.(fieldidx) ne (*self_struct.(origidx)).(fieldidx) then begin
        self_struct.(touchedIdx[i]) = 1
      endif
    endelse
            
  endfor
  
  self->SetAll,self_struct
  
end

;General setProperty method,
;Like IDL is allows partial keyword names, as long as the stem is not ambiguous
;It maintains some reserved syntax, any keyword named 'notouched' cannot be used to set a property with this method.
;If the keyword notouched is present, it will treat this as an instruction to skip any sets for properties, if the object
;has another property called touchedpropertyname, which is set to true.
;
;For example,
;  An object may have the following members:
;  struct = { EXAMPLECLASS,test:0,touchedtest:1,inherits thm_ui_getset }
;  and the following code:
;  exampleobj = obj_new('exampleclass')
;  exampleobj->setProperty,test=7,/notouched
;  Does nothing.
;  But,
;  exampleobj->setProperty,touchedtest=0,test=7,/notouched
;  Sets test = 7, and touchedtest = 0
;  exampleobj->setProperty,test=7,touchedtest=1
;  Sets test = 7 and touchedtest=1 (because notouched was not set) 
;
pro thm_ui_getset::setProperty,_extra=ex
 
  compile_opt idl2
 
  if ~keyword_set(ex) then return
 
  in_tg = strlowcase(tag_names(ex))
  
  notouched = 0
  
  ;filter notouched keyword
  if in_set(in_tg,'notouched') then begin
    notouched = ex.notouched
    str_element,ex,'notouched',/delete
    in_tg = strlowcase(tag_names(ex))
  endif
  
  self_struct = self->getAll()
  
  self_tg = strlowcase(tag_names(self_struct))
  
  for i = 0,n_elements(in_tg)-1 do begin
  
    ;uses regular expression so that partially matching keywords are permitted
    idx = where(stregex(self_tg,'^'+in_tg[i],/boolean),c)
    
    if c eq 0 then begin
      message,'Illegal keyword passed to setProperty: ' + in_tg[i]
    endif else if c gt 1 then begin
      message,'Ambiguous keyword passed to setProperty: ' + in_tg[i]
    endif
    
    ;skip any fields that have the touched field set to true
    if keyword_set(notouched) then begin
    
      idx_touched_self = where(stregex(self_tg,'^touched'+in_tg[i],/boolean),c_self)
      
      if c_self eq 1 then begin
      
        idx_touched_in = where(stregex(in_tg,'^touched'+in_tg[i],/boolean),c_in)
        
        if c_in eq 1 && ex.(idx_touched_in) then begin
          continue
        endif else if self_struct.(idx_touched_self) then begin
          continue
        endif
      endif      
  
    endif
  
    ;If types don't match IDL will naturally and correctly throw an error
    ;So I don't bother doing the check myself
   
    ;This if statement makes sure only valid object assignments are allowed
    if size(ex.(i),/type) ne 11 || obj_valid(ex.(i)) then begin 
      self_struct.(idx) = ex.(i)
    endif
  
  endfor
  
  self->SetAll,self_struct
 
end

;This routine is slightly more awesome than standard getProperty
;Because it allows use of partial keywords.  A word of warning though,
;If you use this incorrectly, you may find the wrong variable in the parent
;code modified
pro thm_ui_getset::getProperty,_ref_extra=in_tg

  compile_opt idl2

  if ~keyword_set(in_tg) then return
  
  self_struct = self->getAll()
  
  self_tg = strlowcase(tag_names(self_struct))
  in_tg = strlowcase(in_tg)
  
  for i = 0,n_elements(in_tg)-1 do begin
  
    ;uses regular expression so that partially matching keywords are permitted
    idx = where(stregex(self_tg,'^'+in_tg[i],/boolean),c)
    
    if c eq 0 then begin
      message,'Illegal keyword passed to getProperty: ' + in_tg[i]
    endif else if c gt 1 then begin
      message,'Ambiguous keyword passed to getProperty: ' + in_tg[i]
    endif
  
    (scope_varfetch(in_tg[i],/ref_extra))=self_struct.(idx)      
  
  endfor

 
end

function thm_ui_getset::init

  return,1

end

PRO thm_ui_getset__define

   struct = { thm_ui_getset,___dummy:0 } ;this used 3 underscores to make it distinct from thm_ui_readwrite which uses 2

END
