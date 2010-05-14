
;+
;
;Procedure: mini_routines
;
;Purpose:  Compiles a bunch of routines used to describe the evaluation rules
;          used by the language.  productions.pro actually describes which syntactical
;          rules use each of the routines in this file. Also see the routines 'function_list' in
;          this file to see which names get bound to which routines in the language
;
;TODO: 1. need to include linear algebraic functions in the set of available routines
;        (crossp,norm,normalize), also multivariable calculus functions(gradient,curl)
;        /nan flag set whenever possible, & statistical routines, skew,kurtosis,variance,stddev
;
;      2. consider putting function/operator list inside common block
;
; NOTES:
;      these routines are intentionally designed to preserve type
;      i.e. not upgrade float to double or short to long unless required
;      It leaves decisions about type to the evaluator and/or user 
;      trigonometric routines will transform inputs into floating point,
;      however
;
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-08-13 15:10:57 -0700 (Thu, 13 Aug 2009) $
; $LastChangedRevision: 6572 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/mini/mini_routines.pro $
;- 

function mini_log,x,b

  compile_opt hidden,strictarr

  out = x

  if ~keyword_set(b) then begin
    out.data = alog10(x.data)
  endif else begin
    out.data = alog10(x.data) / alog10(b.data)
  endelse
  
  return,out

end

function mini_ln,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = alog(x.data)
  
  return,out
  
end

function mini_exp,x,b

  compile_opt hidden,strictarr
  
  out = x
  
  if ~keyword_set(b) then begin
    out.data = exp(x.data)
  endif else begin
    out.data = b.data ^ x.data
  endelse
  
  return,out
  
end

function mini_sqrt,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = sqrt(x.data)
  
  return,out
  
end
  
function mini_abs,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = abs(x.data)
  
  return,out
  
end
  
function mini_sin,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = sin(x.data)
  
  return,out
  
end 

function mini_asin,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = asin(x.data)
  
  return,out
  
end 

function mini_sinh,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = sinh(x.data)
  
  return,out
  
end 

function mini_cos,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = cos(x.data)
  
  return,out
  
end 

function mini_acos,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = acos(x.data)
  
  return,out
  
end 

function mini_cosh,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = cosh(x.data)
  
  return,out
  
end 

function mini_tan,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = tan(x.data)
  
  return,out
  
end 

function mini_atan,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = atan(x.data)
  
  return,out
  
end 

function mini_tanh,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = tanh(x.data)
  
  return,out
  
end 

function mini_cosecant,x

  compile_opt hidden,strictarr

  out = x

  out.data = 1/sin(x.data)
  
  return,out

end

function mini_arccosecant,x

  compile_opt hidden,strictarr

  out = x

  out.data = asin(1/x.data)
  
  return,out

end

function mini_cosecanthyp,x

  compile_opt hidden,strictarr

  out = x
  
  out.data = 1/sinh(x.data)
  
  return,out
  
end

function mini_secant,x

  compile_opt hidden,strictarr

  out = x
  
  out.data = 1/cos(x.data)
  
  return,out

end

function mini_arcsecant,x

  compile_opt hidden,strictarr
  
  out = x

  out.data = acos(1/x.data)
  
  return,out

end

function mini_secanthyp,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = 1/cosh(x.data)
  
  return,out
  
end

function mini_cotangent,x

  compile_opt hidden,strictarr
  
  out = x

  out.data = cos(x.data)/sin(x.data)
  
  return,out

end

function mini_arccotangent,x

  compile_opt hidden,strictarr
  
  out = x
  
  if is_num(x.data,/double) then begin ;prevent accidental type upgrade
    pi = !DPI
  endif else begin
    pi = !PI
  endelse
  
  if ~is_array(x.data) then begin  ;single element case
    if x.data lt 0 then begin
      out.data = atan(1/x.data)+pi
    endif else begin
      out.data = atan(1/x.data)
    endelse
  endif else begin  ;array case
  
    out.data = atan(1/x.data)
    
    idx = where(x.data lt 0)
    
    if idx[0L] ne -1 then begin
      out.data[idx] += pi
    endif
 
  endelse
  
  return,out
      
end

function mini_cotangenthyp,x

  compile_opt hidden,strictarr
  
  out = x
  
  out.data = cosh(x.data)/sinh(x.data)
  
  return,out
  
end

function mini_min,x,d

  compile_opt hidden,strictarr
   
  if keyword_set(d) then begin 
    data = dim_correct_data(min(x.data,dim=d.data,/nan),ndimen(x.data),d.data)
  endif else begin
    data = min(x.data,/nan)
  endelse

  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data),d),d),'min',d)
   
end

function mini_max,x,d

  compile_opt hidden,strictarr
  
  if keyword_set(d) then begin
    data = dim_correct_data(max(x.data,dim=d.data,/nan),ndimen(x.data),d.data)
  endif else begin
    data = max(x.data,/nan)
  endelse
  
  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data),d),d),'max',d)

end

function mini_mean,x,d

  compile_opt hidden,strictarr

  if keyword_set(d) then begin
    data = dim_correct_data(average(x.data,d.data,/nan),ndimen(x.data),d.data)
  endif else begin
    data = average(x.data,/nan)
  endelse
  
  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data),d),d),'mean',d)

end

function mini_median,x,d

  compile_opt hidden,strictarr

  if keyword_set(d) then begin
    data = dim_correct_data(median(ndimen(x.data)?x.data:[x.data],dim=d.data,/even),ndimen(x.data),d.data)
  endif else begin
    data = median(ndimen(x.data)?x.data:[x.data],/even)
  endelse
    
  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data),d),d),'median',d)

end

function mini_count,x,d

  compile_opt hidden,strictarr

  if keyword_set(d) then begin
    data = (size(x.data,/dimensions))[d.data-1L]
  endif else begin  
    data = n_elements(x.data)
  endelse
  
  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data))),'#',d)
 
end

function mini_total,x,d
    
  compile_opt hidden,strictarr
  
  if keyword_set(d) then begin
    data = dim_correct_data(total(x.data,d.data,/nan),ndimen(x.data),d.data)
  endif else begin
    data = total(x.data,/nan)
  endelse
  
  return,reduce_dlimits(reduce_yvalues(reduce_times(replace_data(x,data),d),d),'total',d)
  
 
end

function mini_return,arg

  compile_opt hidden,strictarr
  
  if ~keyword_set(arg) then begin
    message,'No arg passed to mini_return'
  endif
  
  return,arg
  
end

function mini_number,arg

  compile_opt hidden,strictarr
  
  if ~keyword_set(arg) then begin
    message,'No arg passed to mini_return'
  endif
  
  return,{type:'literal_data',name:arg.name,data:arg.value}
  
end

;This routine breaks token abstraction for operators/assignments
;It should be fixed
function mini_assign,arg1,arg2,arg3

  compile_opt hidden,strictarr
  
  evaluator_routines
  mini_predicates
  
  if ~is_assignment_type(arg2) then begin
    message,'Arg2 to mini_assign not assignment type'
  endif
  
  if arg2.value ne '=' then begin
    
    op = arg2
    
    op.type = 'operator'
    op.name = op.value
    
    var = mini_var(arg1)
    
    value = mini_bop(var,op,arg3)
    
  endif else begin
  
    value = arg3
    
  endelse
  
  store_var_data,arg1,value
  
  return,{type:'empty'}
  
end

function mini_var,arg1

  compile_opt hidden,strictarr
  
  evaluator_routines
  mini_predicates
  
  if is_tvar_type(arg1) then begin
  
    if obj_valid(!mini_globals.gui_data_obj) then begin
      obj = !mini_globals.gui_data_obj->getTvarObject(arg1.value)
      
      if ~obj_valid(obj) then begin
        message,'error reading gui variable: ' + arg1.value
      endif
      
      get_data,arg1.value,data=d,limit=l,dlimit=dl
      
      ;quick hack to implement gui variables
      ;I store the inherited meta-data in the dlimits
      dl = {object:obj,dlimits:dl}
    endif else begin
  
      get_data,arg1.value,data=d,limit=l,dlimit=dl
    
      if ~is_struct(d) then begin
        message,'error reading tplot variable: ' + arg1.value
      endif
      
    endelse
    
    return,make_tvar_data(arg1.value,d,l,dl)
    
  endif else if is_var_type(arg1) then begin
  
    varnames = scope_varname(level=!mini_globals.scope_level)
    
    idx = where(strupcase(arg1.value) eq varnames,c)
    
    if ~in_set(strupcase(arg1.value),varnames) then begin
      message,'error reading variable: ' + arg1.value
    endif
  
    ret_val = {type:'var_data',name:arg1.value,data:scope_varfetch(arg1.value,level=!mini_globals.scope_level)}
    
  endif else begin
  
    message,'mini_var passed illegal argument type'
 
  endelse
 
  return,ret_val
  
end

function mini_incdec,arg1,arg2

  compile_opt hidden,strictarr
  
  evaluator_routines
  mini_predicates
  
  if is_operator_type(arg1) then begin
  
    out_var = mini_var(arg2)   
  
    if arg1.value eq '++' then begin

      out_var.data++
      
   endif else if arg1.value eq '--' then begin
      
      out_var.data--

    endif else begin
      message,"wrong operator passed to mini_incdec"
    endelse
    
    store_var_data,arg2,out_var
    
  endif else if is_operator_type(arg2) then begin
     
    out_var = mini_var(arg1)
     
    store_var = out_var
     
    if arg2.value eq '++' then begin
 
      store_var.data++
 
    endif else if arg2.value eq '--' then begin
    
      store_var.data--
       
    endif else begin
      message,'wrong operator passed to mini_incdec'
    endelse
    
    store_var_data,arg1,store_var
    
  endif else begin
    message,'no operator passed to mini_incdec'
  endelse
  
  return,out_var
  
end

function mini_uop,arg1,arg2

  compile_opt hidden,strictarr
  
  evaluator_routines
  mini_predicates
  
  if ~is_operator_type(arg1) then begin
    message,'arg1 to mini_uop not operator'
  endif
  
  if arg1.value eq '+' then begin
  
      out = arg2
      out.data = +out.data
      
  endif else if arg1.value eq '~' then begin

      out = arg2
      out.data = ~out.data

  endif else if arg1.value eq '-' then begin

      out = arg2
      out.data = -1*out.data

  endif else if arg1.value eq 'not' then begin

      out = arg2
      out.data = not out.data

  endif else begin
    message,'illegal operator passed to mini_uop'
  endelse
  
  return,out
  
end
  
function mini_paren,arg1,arg2,arg3

  compile_opt hidden

  return,arg2

end

function mini_func,arg1,arg2,arg3,arg4

  compile_opt hidden,strictarr
  
  mini_predicates
  
  if is_empty_type(arg3) then begin
    return,call_function(arg1.value)
  endif else if arg3.length eq 1 then begin
    return,call_function(arg1.value,arg3.data)
  endif else if arg3.length eq 2 then begin
    return,call_function(arg1.value,arg3.data,arg3.next.data)
  endif else begin
    message,'wrong number of arguments to mini_func'
  endelse

end

function mini_bop,arg1,arg2,arg3

  compile_opt hidden,strictarr
  
  evaluator_routines
  
  if is_tvar_data(arg1) && is_tvar_data(arg3) && $
     in_set('times',strlowcase(tag_names(arg1))) && in_set('times',strlowcase(tag_names(arg3))) && $
     ~(logical_xor(n_elements(arg1.times) eq 1,n_elements(arg3.times) eq 1)) then begin 
  
    ;don't treat NaNs as always false when checking array equivalnency
    idx1 = where(finite(arg1.times,/nan),complement=cidx1,ncomplement=ncidx1)
    idx3 = where(finite(arg3.times,/nan),complement=cidx3,ncomplement=ncidx3)
  
    if ~array_equal(idx1,idx3) || $
       ncidx1 ne ncidx3 || $
       (ncidx1 gt 0 && ~array_equal(arg1.times[cidx1],arg3.times[cidx3])) then begin
      message,'times in tvar "' +arg1.name + '" and tvar "' + arg3.name + '" do not match'
    endif
  
    ;if ~array_equal(arg1.times,arg3.times) then begin
    ;  message,'times in tvar "' +arg1.name + '" and tvar "' + arg3.name + '" do not match'
    ;endif
    
  endif
  
  if ~is_valid_bop_arg(arg1,arg2,arg3) then begin
    message,'The dimensions of "' + arg1.name + '" and "' + arg3.name + '" do not match'
  endif
  
  if arg2.name eq '^' then begin
    out = arg1.data ^ arg3.data
  endif else if arg2.name eq '*' then begin
    out = arg1.data * arg3.data
  endif else if arg2.name eq '#' then begin
    out = arg1.data # arg3.data
  endif else if arg2.name eq '##' then begin
    out = arg1.data ## arg3.data
  endif else if arg2.name eq '/' then begin
    out = arg1.data / arg3.data
  endif else if arg2.name eq 'mod' then begin
    out = arg1.data mod arg3.data
  endif else if arg2.name eq 'b+' then begin
    out = arg1.data + arg3.data
  endif else if arg2.name eq 'b-' then begin
    out = arg1.data - arg3.data
  endif else if arg2.name eq '<' then begin
    out = arg1.data < arg3.data  
  endif else if arg2.name eq '>' then begin
    out = arg1.data > arg3.data 
  endif else if arg2.name eq 'eq' then begin
    out = arg1.data eq arg3.data 
  endif else if arg2.name eq 'ne' then begin
    out = arg1.data ne arg3.data  
  endif else if arg2.name eq 'le' then begin
    out = arg1.data le arg3.data  
  endif else if arg2.name eq 'lt' then begin
    out = arg1.data lt arg3.data  
  endif else if arg2.name eq 'ge' then begin
    out = arg1.data ge arg3.data   
  endif else if arg2.name eq 'gt' then begin
    out = arg1.data gt arg3.data    
  endif else if arg2.name eq 'and' then begin
    out = arg1.data and arg3.data   
  endif else if arg2.name eq 'or' then begin
    out = arg1.data or arg3.data   
  endif else if arg2.name eq 'xor' then begin
    out = arg1.data xor arg3.data   
  endif else if arg2.name eq '&&' then begin
    out = logical_and(arg1.data,arg3.data)
  endif else if arg2.name eq '||' then begin
    out = logical_or(arg1.data,arg3.data)
  endif else begin
    message,'Unrecognized operator passed to mini_bop'
  endelse
     
  ;not really sure what constitutes a good rule for limit inheritance
  if is_tvar_data(arg1) then begin
    out_var = arg1
    out_var.name = 'composite'
    return,replace_data(out_var,out)
  endif else if is_tvar_data(arg3) then begin
    out_var = arg3
    out_var.name = 'composite'
    return,replace_data(out_var,out)
  endif else begin
    return,make_var_data('composite',out)
  endelse
  
end

function mini_empty,arg1

  compile_opt hidden

  return,{type:'empty',length:0}
  
end

function mini_arg,arg1

  compile_opt hidden

  return,{type:'arg_list',data:arg1,length:1,next:''}

end

function mini_args,arg1,arg2,arg3

  compile_opt hidden
  
  return,{type:'arg_list',data:arg1,length:(arg3.length+1),next:arg3}

end

;Not used directly to evaluate. Function list returns a list of functions and
;the 
;collection of function/procedures related to running various routines in the mini
;consider putting this is common block
function function_list

  compile_opt idl2,hidden

  f_names = [$  ;list of names as they appear in mini-language code
  'log',$
  'ln',$
  'exp',$
  'sqrt',$
  'abs',$
  'sin',$
  'arcsin',$
  'sinh',$
  'cos',$
  'arccos',$
  'cosh',$
  'tan',$
  'arctan',$
  'tanh',$
  'csc',$
  'arccsc',$
  'csch',$
  'sec',$
  'arcsec',$
  'sech',$
  'cot',$
  'arccot',$
  'coth',$
  'min',$
  'max',$
  'mean',$
  'median',$
  'count',$
  'total']  
  
  f_values = [$  ;list of names as they will be called by 'call_function'
  'mini_log',$
  'mini_ln',$
  'mini_exp',$
  'mini_sqrt',$
  'mini_abs',$
  'mini_sin',$
  'mini_asin',$
  'mini_sinh',$
  'mini_cos',$
  'mini_acos',$
  'mini_cosh',$
  'mini_tan',$
  'mini_atan',$
  'mini_tanh',$
  'mini_cosecant',$
  'mini_arccosecant',$
  'mini_cosecanthyp',$
  'mini_secant',$
  'mini_arcsecant',$
  'mini_secanthyp',$
  'mini_cotangent',$
  'mini_arccotangent',$
  'mini_cotangenthyp',$
  'mini_min',$
  'mini_max',$
  'mini_mean',$
  'mini_median',$
  'mini_count',$
  'mini_total']
  
  fun = {type:'function',name:'name',value:'idlname',index:0}

  f_list = replicate(fun,n_elements(f_names))
  
  f_list[*].name = f_names
  f_list[*].value = f_values
  f_list[*].index = lindgen(n_elements(f_names))
  
  return,f_list

end

;consider putting this is common block
function operator_list

  compile_opt idl2,hidden

  op_names = [$  ;list of names as they appear in mini-language code
    '~',$
    '++',$
    '--',$
    'u-',$
    'b-',$
    'u+',$
    'b+',$
    '*',$
    '/',$
    '^',$
    '<',$
    '>',$
    '&&',$
    '||',$
    '#',$
    '##',$
    'mod',$
    'and',$
    'eq',$
    'ge',$
    'gt',$
    'le',$
    'lt',$
    'or',$
    'xor']
    
  op_values = [$ ;list of names as they will be called by 'call_function'
    'mini_not',$
    'mini_increment',$
    'mini_decrement',$
    'mini_uminus',$
    'mini_bminus',$
    'mini_uplus',$
    'mini_bplus',$
    'mini_multiply',$
    'mini_divide',$
    'mini_power',$
    'mini_less',$
    'mini_greater',$
    'mini_and_logical',$
    'mini_or_logical',$
    'mini_matrix_column',$
    'mini_matrix_row',$
    'mini_mod',$
    'mini_and_vector',$
    'mini_or_vector',$
    'mini_eq',$
    'mini_ge',$
    'mini_gt',$
    'mini_le',$
    'mini_lt',$
    'mini_xor']
    
  ;As it turns out, most of this stuff doesn't matter for operators
  ;Instead operators are handled by 
  op = {type:'operator',name:'name',value:'idlname',index:0}
  
  op_list = replicate(op,n_elements(op_names))
  
  op_list[*].name = op_names
  op_list[*].value = op_values
  op_list[*].index = lindgen(n_elements(op_names))
  
  return,op_list

end

function get_function,tok

  compile_opt idl2,hidden
  
  fl = function_list()
  
  idx = where(tok.name eq fl.name)
  
  if idx[0] eq -1L then begin
    return,''
  endif else if n_elements(idx) gt 1 then begin
    return,''
  endif else begin
    return,fl[idx]
  endelse

end

pro mini_routines

;do nothing

end
