;+
;Function: tinterpol_mxn
;
;Purpose:  Generalized interpolates across a series of vectors(since
;interpol only works on 1-d vectors) This function works on any
;n-dimensional vectors. Basically, this is an element by element
;interpolation across M
;
; - i. works on tplot variables
;     - ii. allows no extrapolation beyond valid data with
;          /no_extrapolate keyword"
;              
;Arguments:
;            xv_tvar = tplot variable to be interpolated, the y component
;            can have any dimesions, can use globbing to interpolate
;            many values at once
;            uses x component for x abcissa values
;            
;            uz_tvar = tplot variable that V will be fit to
;            uses x component for u abcissa values.  Can also
;            pass in an array of time values rather than a tplot 
;            variable.
;            
;            newname = output variable name(optional) defaults to
;            xv_tvar+'_interp'
;            
;            suffix = a suffix other than interp you can use,
;            particularily useful when using globbing
;            
;            overwrite=set this variable if you just want
;            the original variable overwritten instead of using
;            newname or suffix
;
;            Use only newname or suffix or overwrite. If you combine
;            them the naming behavior may be erratic
;
;            /LINEAR = pass this argument to specify linear
;            interpolation(this is the default behavior)
;            
;            /QUADRATIC = pass this argument to specify quadratic
;            interpolation
;            
;            /SPLINE = pass this argument to specify spline
;            interpolation
;            
;            /NO_EXTRAPOLATE = pass this argument to prevent
;            extrapolation of data values in V passed it's start and
;            end points
;            
;            /NAN_EXTRAPOLATE = pass this argument to extrapolate past
;            the endpoints using NaNs as a fill value
;
;            ERROR(optional): named variable in which to return the error state
;            of the computation.  1 = success 0 = failure
;
;Outputs(optional):
;   out:
;     Returns output as a data struct. If this argument is present, no tplot variable will be created
;     Note that only one result can be returned through this keyword.  
;
;CALLING SEQUENCE;
;           tinterpol_mxn,'tplot_var1','tplot_var2',out_var='tplot_var_out'
;           tinterpol_mxn,'tplot_var1','tplot_var2',/NO_EXTRAPOLATE
;           tinterpol_mxn,'tplot_var1','tplot_var2',/SPLINE
;         
;Output: an N by D1 by D2 by ... array stored in an output tplot variabel
;
;Notes: 
;Uses a for loop over D1*D2*..., but I'm operating under the assumption that
;D1*D2... << M (D1 * D2 *... is waaaay less than M)
;
;It uses a little bit of modular arithmatic so this function is
;generalized to any array dimensionality(IDL limits at 8)
;
;
;Examples:
; if the input is an array of 3-d vectors(say 1,1,1 and 2,2,2) and we
; want 3 vectors out the output is 1,1,1 1.5 1.5 1.5 2,2,2
; if the input is an array of 3x3 matrices(say all ones and all twos) 
; and we want three matrices then output is all 1s all 1.5s all 2s 
; 
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-12-09 14:39:12 -0800 (Wed, 09 Dec 2009) $
; $LastChangedRevision: 6983 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/special/tinterpol_mxn.pro $
;-


;helper function
;actually does the bulk of the work
function ctv_interpol_vec_mxn,v,x,u,_extra=_extra

COMPILE_OPT HIDDEN

n = n_elements(u)

if n le 0 then return,-1L

;if the value is atomic return it
if(size(v,/n_dim) eq 0) then begin 
    error=1
    return,replicate(v,n_elements(u))
endif

v_s = size(v,/dimensions)

;handle single input case...it should extrapolate a constant matrix
if(v_s[0] eq 1) then begin
    v_s[0] = 2
    v = rebin(v,v_s)
    x = rebin([x],2)
    x[1] = x[0] + 1.0 ;so the timeseries ascends
endif

;I think I actually handled the 1 case generally
;if(n_elements(v_s) eq 1) then return,interpol(v,n)

v_s_o = v_s

v_s_o[0] = n

out = dindgen(v_s_o)

;the transpose and the reverse make the indexing scheme work out
;cause the in variables(and tplot variables) work more or less in row
;row major, but idl indexes column major
out_idx = transpose(lindgen(reverse(v_s_o)))

in_idx = transpose(lindgen(reverse(v_s)))

;calculate the number of elements in each matrix/vectors/whatever

product = 1

if n_elements(v_s) gt 1 then begin
  product = product(v_s[1:*])
endif

;for i = 1,n_elements(v_s) - 1L do begin
;
;    product *= v_s[i]
;
;endfor

for i = 0,product-1L do begin

    idx1 = where((out_idx mod product) eq i)
    idx2 = where((in_idx mod product) eq i)

    if(size(idx1,/n_dim) eq 0 || $
       n_elements(idx1) ne n || $
       size(idx2,/n_dim) eq 0 || $
       n_elements(idx2) ne v_s[0]) $
       then return, -1L

    if not keyword_set(u) then $
      out[idx1] = interpol(v[idx2],n,_extra=_extra) $
    else $
      out[idx1] = interpol(v[idx2],x,u,_extra=_extra)

endfor

return,out

end

;This helper function fills rows of dat with nans.
;idx is the rows to be filled.  dat can have any number of dimensions
pro ctv_nan_fill_idx,dat,idx

  compile_opt hidden

  if idx[0] eq -1 then return
  
  dat_dim = dimen(dat)
  
  if n_elements(dat_dim) eq 1 then begin
    dat[idx] = !VALUES.D_NAN
  endif else begin
    multiplier = product(dat_dim[1:*]) 
    nan_idx = rebin(idx,n_elements(idx),multiplier)+dat_dim[0]*transpose(lindgen(multiplier,n_elements(idx)) mod multiplier)
    dat[nan_idx] = !VALUES.D_NAN
  endelse

end

pro tinterpol_mxn, xv_tvar, uz_tvar, newname = newname,no_extrapolate = no_extrapolate,nan_extrapolate=nan_extrapolate,error=error,suffix=suffix,overwrite=overwrite,out=out_d,  _extra = _extra

error=0

if not keyword_set(xv_tvar) then begin
  message, /continue, 'xv_tvar must be set for tinterpol_mxn to work'
  return
endif

tn = tnames(xv_tvar)

if size(tn,/n_dim) eq 0 && tn eq '' then begin
  message, /continue, 'xv_tvar must be set for tinterpol_mxn to work'
  return
endif

if not keyword_set(uz_tvar) then begin
  message, /continue, 'uz_tvar must be set for tinterpol_mxn to work'
  return
endif

if is_string(uz_tvar) then begin

  tn_match = tnames(uz_tvar)

  if tn_match eq '' then begin
    message, /continue, 'uz_tvar must be set for tinterpol_mxn to work'
    return
  endif
  
  get_data, tn_match, data = match_d
  
  match_d_x = match_d.x
  
endif else begin

  match_d_x = uz_tvar
  
endelse

;these naming keywords can interfere
;it is left to the user not to use them simultaneously

if keyword_set(suffix) then begin 
   newname = [xv_tvar+suffix] 
endif else if keyword_set(newname) then begin
   newname = [newname]
endif else if keyword_set(overwrite) then begin
   newname = [xv_tvar]
endif else begin
   newname = [xv_tvar + '_interp']
endelse


if n_elements(tn) gt 1 then begin

   if keyword_set(suffix) then begin
      newname = tn+suffix
   endif else begin
      newname = tn+'_interp'
   endelse

   if keyword_set(overwrite) then begin
      newname = tn
   endif

endif

for i = 0, n_elements(tn) -1L do begin

   get_data,tn[i], data = in_d, limits = in_l, dlimits = in_dl

   if keyword_set(no_extrapolate) then begin

      in_min = min(in_d.x,max=in_max)

      idx = where(match_d_x ge in_min and match_d_x le in_max)

      if idx[0] eq -1L then begin
         message, /continue, 'tinterpol_mxn cannot interpolate any values without extrapolating, skipping'

         continue

      endif

      match_d_x = match_d_x[idx]

   endif else if keyword_set(nan_extrapolate) then begin

      in_min = min(in_d.x,max=in_max)

      nan_idx = where(match_d_x lt in_min or match_d_x gt in_max)
       
;replacing NaN extrapolate technique.  Even if abcissa values matched exactly, it
;would end up turning the final point into a nan
;      in_d_x = in_d.x
;      in_d_y = in_d.y
;
;      x_min = min(in_d_x,/nan,max=x_max)
;
;      pad_step = (x_max-x_min)*(is_num(is_d_x,/double)?1d-15:1d-7)
;
;      new_start = in_d_x[0] - pad_step
;      new_end = in_d_x[n_elements(in_d_x)-1L]+pad_step
;
;      in_d_x = [new_start,in_d_x,new_end]
;
;      in_d_y_sz = size(in_d_y,/dimensions)
;
;      in_d_y_sz[0] = 1
;
;      in_d_y = [replicate(!VALUES.D_NAN,in_d_y_sz),in_d_y,replicate(!VALUES.D_NAN,in_d_y_sz)]
;
;      in_d = {x:in_d_x,y:in_d_y}

   endif

   out_d_y = ctv_interpol_vec_mxn(in_d.y, in_d.x, match_d_x, _extra = _extra)

   if(size(out_d_y,/n_dim) eq 0 && out_d_y[0] eq -1L) then begin

      message,/continue,'TINTERPOL_MXN: interpolation Y-component calculation failed'

      return

   endif
   
   if keyword_set(nan_extrapolate) then begin
   
     ctv_nan_fill_idx,out_d_y,nan_idx
   
   endif

   str_element,in_d,'v',success=s

   if s eq 1 then begin
   
     v_dim = dimen(in_d.v)
     
     y_dim = dimen(in_d.y)
     
     if is_num(in_d.v) && n_elements(v_dim) eq n_elements(y_dim) && v_dim[0] eq y_dim[0] then begin
     
       out_d_v = ctv_interpol_vec_mxn(in_d.v, in_d.x, match_d_x, _extra = _extra)
       
       if(size(out_d_y,/n_dim) eq 0 && out_d_y[0] eq -1L) then begin
       
         message,/continue,'TINTERPOL_MXN: interpolation V-component calculation failed'
         
         return
         
       endif
       
       if keyword_set(nan_extrapolate) then begin
   
         ctv_nan_fill_idx,out_d_y,nan_idx
   
       endif
      
       out_d = {x:match_d_x, y:out_d_y, v:out_d_v}
      
     endif else begin
      
       out_d = {x:match_d_x, y:out_d_y, v:in_d.v}
       
     endelse 
   endif else begin
   
     out_d = {x:match_d_x,y:out_d_y}
     
   endelse

   if ~arg_present(out_d) then begin
     store_data, newname[i], data = out_d, limits = in_l, dlimits = in_dl
   endif

endfor

error=1

return

end
