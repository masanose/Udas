
;+
; Procedure: evaluator_routines
;
; Purpose: When called this routine compiles a library of helper routines
;          for the evaluator of the mini_language
;           
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-07-28 11:35:53 -0700 (Tue, 28 Jul 2009) $
; $LastChangedRevision: 6500 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/mini/evaluator_routines.pro $
;- 

;constructs a tvar data struct for mini_routines used in evaluation
function make_tvar_data,name,data,limits,dlimits

  compile_opt hidden

  if in_set('x',strlowcase(tag_names(data))) then begin
     x = data.x
  endif else begin
     x = ''
  endelse

  if in_set('y',strlowcase(tag_names(data))) then begin
     y = data.y
  endif else begin
     message,'Y component of tplot variable missing.  Name: ' + name
  endelse

  if in_set('v',strlowcase(tag_names(data))) then begin
     v = data.v
  endif else begin
     v = ''
  endelse

  if is_equal(limits,0) then begin
     l = ''
  endif else begin
     l = limits
  endelse

  if is_equal(dlimits,0) then begin
     dl = ''
  endif else begin
     dl = dlimits
  endelse

  return,{type:'tvar_data',name:name,times:x,data:y,yvalues:v,limits:l,dlimits:dl}

end

;constructs a var data struct for mini_routines used in evaluation
function make_var_data,name,data

  compile_opt hidden

  return,{type:'var_data',name:name,data:data}

end

;turns a tvar data type from evaluation into a data component for use with store_data
function make_data_type,tvar_type

  compile_opt hidden

  if ~is_tvar_data(tvar_type) || (is_equal(tvar_type.yvalues,'') && is_equal(tvar_type.times,'')) then begin
     
     return,{y:tvar_type.data}

  endif else if is_equal(tvar_type.yvalues,'') then begin

     return,{x:tvar_type.times,y:tvar_type.data}

  endif else if is_equal(tvar_type.times,'') then begin

     return,{y:tvar_type.data,v:tvar_type.yvalues}

  endif else begin

     return,{x:tvar_type.times,y:tvar_type.data,v:tvar_type.yvalues}

  endelse

end

;turns a tvar data type from evaluation into a limits component for use with store_data
function make_limits_type,tvar_type

  compile_opt hidden

  if ~is_tvar_data(tvar_type) || is_equal(tvar_type.limits,'') then begin
     return,0
  endif else begin
     return,tvar_type.limits
  endelse

end

;turns a tvar data type from evaluation into a dlimits component for use with store_data
function make_dlimits_type,tvar_type

  compile_opt hidden

  if ~is_tvar_data(tvar_type) || is_equal(tvar_type.dlimits,'') then begin
     return,0
  endif else begin
     return,tvar_type.dlimits
  endelse

end

;replaces the data component of variable or tplot variable
;This is for use in cases where the new data compnent may have
;different dimensions
function replace_data,var,data

  compile_opt hidden
  
  if is_var_data(var) then begin
    return,make_var_data(var.name,data)
  endif else begin
    return,{type:var.type, $
            name:var.name, $
            times:var.times, $
            data:data,$
            yvalues:var.yvalues,$
            limits:var.limits,$
            dlimits:var.dlimits}
  endelse
  
end

;performs a dimensional correction so
;that operations that would collapse the leading
;dimension transform from MxN to 1xN, this
;makes handling elsewhere more consistent
function dim_correct_data,data,xdim,opdim

  compile_opt hidden

    if opdim eq 1 && xdim gt 1 then begin
      return,reform(data,[1,dimen(data)])
    endif else begin
      return,data
    endelse

end

;Certain operations will result in fewer dimensions.
;If there are associated times, these must
;have a corresponding reduction for the quantity to remain
;well-formed
function reduce_times,var,dim

  compile_opt hidden
  
  if is_tvar_data(var) && ~is_equal(var.times,'') && $   
     (~keyword_set(dim) || dim.data eq 0 || dim.data eq 1) then begin
    
    return,{type:var.type, $
            name:var.name, $
            times:ndimen(var.times)?median(var.times,/even):median([var.times],/even), $
            data:var.data,$
            yvalues:var.yvalues,$
            limits:var.limits,$
            dlimits:var.dlimits}
    
  endif else begin
  
    return,var
    
  endelse
  
end

;Certain operations will result in fewer dimensions.
;If there are associated yvalues, these must
;have a corresponding reduction for the quantity to remain
;well-formed
function reduce_yvalues,var,dim

  compile_opt hidden
  
  if is_tvar_data(var) && ~is_equal(var.yvalues,'') then begin
    if ~keyword_set(dim) || dim.data eq 0 then begin
      return,{type:var.type, $
              name:var.name, $
              times:var.times, $
              data:var.data,$
              yvalues:ndimen(var.yvalues)?median(var.yvalues,/even):median([var.yvalues],/even),$
              limits:var.limits,$
              dlimits:var.dlimits}    
    endif else if dim.data eq 1 then begin
      if ndimen(var.yvalues) ge 2 then begin
        return,{type:var.type, $
                name:var.name, $
                times:var.times, $
                data:var.data,$
                yvalues:dim_correct_data(median(var.yvalues,dim=1,/even),ndimen(var.yvalues),1),$
                limits:var.limits,$
                dlimits:var.dlimits}
      endif
    endif else if dim.data eq 2 then begin
      if ndimen(var.yvalues) ge 2 then begin
       return,{type:var.type, $
              name:var.name, $
              times:var.times, $
              data:var.data,$
              yvalues:median(var.yvalues,dim=2,/even),$
              limits:var.limits,$
              dlimits:var.dlimits}
      endif else begin
       return,{type:var.type, $
              name:var.name, $
              times:var.times, $
              data:var.data,$
              yvalues:ndimen(var.yvalues)?median(var.yvalues,/even):median([var.yvalues],/even),$
              limits:var.limits,$
              dlimits:var.dlimits}
      endelse
    endif
  endif
  
  return,var

end

;Certain operations will result in fewer dimensions.
;If there are associated limits/dlimits, this
;will reduce the labels/colors
function reduce_dlimits,var,label,dim

  if ~keyword_set(dim) || dim.data ne 2 then return,var  ; only applies if totaling over dim 2

  var_out = var
  dlimits = make_dlimits_type(var)

  if keyword_set(dlimits) then begin
    if in_set(strlowcase(tag_names(dlimits)),'dlimits') then begin
      dl = dlimits.dlimits
    endif else begin
      dl = dlimits
    endelse
    
    if in_set(strlowcase(tag_names(dl)),'colors') && n_elements(dl.colors) gt 1 then begin
      str_element,dl,'colors',0,/add
    endif
    
    if in_set(strlowcase(tag_names(dl)),'labels') && n_elements(dl.labels) gt 1 then begin
      str_element,dl,'labels',label,/add
    endif
    
    if in_set(strlowcase(tag_names(dlimits)),'dlimits') then begin
      str_element,dlimits,'dlimits',dl,/add
    endif else begin
      dlimits = dl
    endelse
    
    str_element,var_out,'dlimits',dlimits,/add
  endif
  
  limits = make_limits_type(var)
  
  if keyword_set(limits) then begin
  
    if in_set(strlowcase(tag_names(limits)),'colors') && n_elements(limits.colors) gt 1 then begin
      str_element,limits,'colors',0,/add
    endif
    
    if in_set(strlowcase(tag_names(limits)),'labels') && n_elements(limits.labels) gt 1 then begin
      str_element,limits,'labels',label,/add
    endif
    
    str_element,var_out,'limits',limits,/add
  
  endif
  
  return,var_out
  
end

;abstraction routine determines whether its input is a tvar type or not
; this is different from a tvar data type 1=yes 0=no
function is_tvar_type,in

  compile_opt idl2,hidden
  
  mini_predicates
  
  if is_string_type(in) then begin
    return,1
  endif else begin
    return,0
  endelse

end

;abstraction routine determines whether its input is a var type or not
;this is different from a var data type 1=yes 0=no
function is_var_type,in 

  compile_opt idl2,hidden
  
  mini_predicates

  if is_identifier_type(in) then begin
    return,1
  endif else begin
    return,0
  endelse
  
end

;abstracts the data storage process
pro store_var_data,name,value

  compile_opt hidden

  if is_tvar_type(name) then begin

  ;  dprint,getdebug=g,verbose=-1
  ;  dprint,setdebug=-1,verbose=-1

    if obj_valid(!mini_globals.gui_data_obj) then begin
       
       data = make_data_type(value)
       limits = make_limits_type(value)
       tmp_limits = make_dlimits_type(value)
       
       dlimits = tmp_limits.dlimits
       obj = tmp_limits.object
       
       obj->setProperty,name=name.value
              
       store_data,name.value,data=data,limits=limits,dlimits=dlimits,error=e

       if e then begin
         message,'store_data error'
       endif
       
       if ~!mini_globals.gui_data_obj->addTvarObject(obj) then begin
         message,'loaded_data error'
       endif

    endif else begin
      store_data,name.value,data=make_data_type(value),limits=make_limits_type(value),dlimit=make_dlimits_type(value),error=e
           
      if e then begin
        message,'store_data error'
      endif
    endelse

  ;  dprint,setdebug=g

  endif else if is_var_type(name) then begin
  
    (scope_varfetch(name.value,/enter,level=!mini_globals.scope_level)) = value.data
   
  endif else begin
  
    message,'Wrong type in store_var_data'
  
  endelse

end

;determines whether an entry in the parse table is an error or not
function is_error_code,in

  compile_opt idl2,hidden
  
  if is_equal(in,'') then begin 
    return,1
  endif else begin
    return,0
  endelse
  
end 
   
;determines whether an entry in the parse table is a shift code or not
function is_shift_code,in 

  compile_opt idl2,hidden 
  
  if strlen(in) gt 1 && strmid(in,0,1) eq 's' then begin
    return,1
  endif else begin
    return,0
  endelse
  
end

;gets the shift code from a parse table entry
function get_shift_num,in

  compile_opt idl2,hidden
  
  if strlen(in) lt 2 then begin
    return,-1
  endif else begin
    return,long(strmid(in,1))
  endelse
  
end

;determines whether an entry in the parse table is a reduce code or not
function is_reduce_code,in 

  compile_opt idl2,hidden 
  
  if strlen(in) gt 1 && strmid(in,0,1) eq 'r' then begin
    return,1
  endif else begin
    return,0
  endelse
  
end

;determines if binary operator arguments are compatible
function is_valid_bop_arg,arg1,arg2,arg3

  compile_opt idl2,hidden
  
  dim1 = dimen(arg1.data)
  dim2 = dimen(arg3.data)
  ndim1 = ndimen(arg1.data)
  ndim2 = ndimen(arg3.data) 
  
  ;scalar exemption
  if ndim1 eq 0 || ndim2 eq 0 || dim1[0] eq 1 || dim2[0] eq 1 then return,1
  
  ;matrix multiplication rules type 1
  if arg2.name eq '#' then begin
  
    if ndim1 eq 1 && ndim2 eq 1 then return,1
    
    if ndim1 eq 1 && ndim2 eq 2 && $
       dim1[0] eq dim2[0] then return,1
    
    if ((ndim1 eq 2 && ndim2 eq 1) || $
       (ndim1 eq 2 && ndim2 eq 2)) && $
       dim1[1] eq dim2[0] then return,1
  
  ;matrix multiplication rules type 2
  endif else if arg2.name eq '##' then begin
  
    if ndim1 eq 1 && ndim2 eq 1 then return,1
    
    if ndim1 eq 2 && ndim1 eq 1 && $
       dim1[0] eq dim2[0] then return,1
       
    if ((ndim1 eq 1 && ndim2 eq 2) || $
        (ndim1 eq 2 && ndim2 eq 2)) && $
        dim1[0] eq dim2[1] then return,1
        
  ;other operation rules
  endif else begin

     if array_equal(dim1,dim2) then return,1  
       
  endelse
       
  return,0

end

;gets the reduce code from a parse table entry
function get_reduce_num,in

  compile_opt idl2,hidden
  
  if strlen(in) lt 2 then begin
    return,-1
  endif else begin
    return,long(strmid(in,1))
  endelse
  
end

;evaluates each token passed to it.
;the most important aspect of this function
;is to translate tokens from the lexer into a format that
;the evaluator can read.  This entails making sure the name component of
;the output structure is a terminal from the language grammar 
function eval_token,token,previous,grammar

  compile_opt idl2,hidden
  
  mini_predicates
  mini_routines
  
  if is_function_type(token) then begin
      
      tk = get_function(token)
            
      if ~is_struct(tk) then begin
        return, {type:'error',name:'function lookup error',value:token.name,position:n}
      endif else begin
      
        tk.name = 'func'
      
        ev = tk
      endelse
      
  endif else if is_var_type(token) then begin
  
    ev = {type:token.type,name:'var',value:token.name,index:0}
     
  endif else if is_tvar_type(token) then begin
  
    ev = {type:token.type,name:'tvar',value:strmid(token.name,1,strlen(token.name)-2),index:0}
    
  endif  else if is_operator_type(token) then begin
    
    ev = token
    
    if token.name eq '-' then begin
      if is_unary_minus(token,previous) then begin 
        ev.name = 'u-'
      endif else begin
        ev.name = 'b-'
      endelse
      ev.value = token.name
    endif else if token.name eq '+' then begin
      if is_unary_plus(token,previous) then begin
        ev.name = 'u+'
      endif else begin
        ev.name = 'b+'
      endelse
      ev.value = token.name
    endif
     
  endif else if is_assignment_type(token) then begin
    
   ev = token 
   
   if strlen(token.value) gt 1 then begin
     ev.value = strmid(token.value,0,1)
   endif
    
   if ev.value eq '-' then begin
     ev.value = 'b-'
   endif else if ev.value eq '+' then begin
     ev.value = 'b+'
   endif
      
  endif else if is_numerical_type(token) then begin
  
    if strpos(token.name,'u') ne -1 then begin
      unsigned = 1
    endif else begin
      unsigned = 0
    endelse
    
    if strpos(token.name,'b') ne -1 then begin
    
      ev = {type:token.type,name:'number',value:byte(token.name),index:0}
      
    endif else if strpos(token.name,'s') ne -1 then begin
    
      if unsigned then begin   
        ev = {type:token.type,name:'number',value:uint(token.name),index:0}        
      endif else begin
        ev = {type:token.type,name:'number',value:fix(token.name),index:0}
      endelse
    
    endif else if strpos(token.name,'d') ne -1 then begin
    
      ev = {type:token.type,name:'number',value:double(token.name),index:0}  
                  
    endif else if strpos(token.name,'e') ne -1 || $
                  strpos(token.name,'.') ne -1 then begin
                  
      ev = {type:token.type,name:'number',value:float(token.name),index:0}
      
    endif else if strpos(token.name,'ll') ne -1 then begin
    
      if unsigned then begin   
        ev = {type:token.type,name:'number',value:ulong64(token.name),index:0}        
      endif else begin
        ev = {type:token.type,name:'number',value:long64(token.name),index:0}
      endelse
    
    endif else if strpos(token.name,'l') ne -1 then begin
      
      if unsigned then begin   
        ev = {type:token.type,name:'number',value:ulong(token.name),index:0}
      endif else begin
        ev = {type:token.type,name:'number',value:long(token.name),index:0}   
      endelse
      
    endif else begin
    
      ev = {type:token.type,name:'number',value:float(token.name),index:0}
      
    endelse
  endif else begin
  
    ev = {type:token.type,name:token.name,value:token.name,index:0}
    
  endelse
  
  idx = where(ev.name eq grammar.terminals)
  
  if idx[0] eq -1 then begin
  
    message,'token should always evaluate to a terminal in language'
    
  endif
  
  ev.index = idx
  
  return,ev
  
end

;this routine just compiles all the routines in this file 
pro evaluator_routines

end
 
