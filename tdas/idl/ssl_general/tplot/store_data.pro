;+
;PROCEDURE:  store_data,name,DATA=data,LIMITS=limits,DLIMITS=dlimits,
;     NEWNAME=newname,DELETE=delete
;PURPOSE:
;   Store time series structures in static memory for later retrieval
;   by the tplot routine.  Three structures can be associated with the
;   string 'name':  a data structure (DATA) that typically contains the x and
;   y data. A default limits structure (DLIMITS) and a user limits structure
;   (LIMITS) that will typically contain user defined limits and options
;   (typically plot and oplot keywords).  The data structure and the default
;   limits structure will be
;   over written each time a new data set is loaded.  The limit structure
;   is not over-written.
;INPUT:
;   name:   string name to be associated with the data structure and/or
;     the limits structure.  Also, can enter tplot index as name.
;     The name should not contain spaces or the characters '*' and '?'
;KEYWORDS:
;    DATA:  variable that contains the data structure.
;    LIMITS; variable that contains the limit structure.
;    DLIMITS; variable that contains the default limits structure.
;    NEWNAME: new tplot handle.  Use to rename tplot names.
;    DELETE: array of tplot handles or indices to delete from common block.
;    MIN: if set, data values less than this value will be made NaN.               (obsolete)
;    MAX: if set, data values greater than this value will be made NaN.            (obsolete)
;    NOSTRSW: if set, do not transpose multidimensional data arrays in
;         structures.  The default is to transpose.
;    ERROR: if set returns error code for store_data, values are:
;    0=NO ERROR
;    1=INVALID HANDLE ERROR
;    2=OTHER ERROR
;
;SEE ALSO:    "GET_DATA", "TPLOT_NAMES",  "TPLOT", "OPTIONS"
;
;CREATED BY:    Davin Larson
;MODIFIED BY:   Peter Schroeder
;VERSION:   @(#)store_data.pro   1.44 02/04/17
;-
pro store_data,name, $
   data = data, $
   limits= limits, $
   dlimits = dlimits, $
   newname = newname, $
   min=min, max=max, $
   delete = delete, $
   verbose = verbose, $
   nostrsw = nostrsw,$
   except_ptrs = except_ptrs, $
   error=error

@tplot_com.pro

error = 0

str_element,tplot_vars,'options.verbose',verbose   ; get default value if it exists

if keyword_set(delete) then begin
   if n_elements(name) ne 0 then delete = name
   delnames = tnames(delete,cnt)
   if cnt ne 0 then begin
         au = array_union(data_quants.name,delnames)
         savevars = where(au eq -1)
         delevars = where(au ne -1)
         saveptrs = ptr_extract(except_ptrs)
         saveptrs = [saveptrs,ptr_extract(data_quants[savevars])]
         delptrs = ptr_extract(data_quants[delevars],except=saveptrs)
         data_quants=data_quants[savevars]
         ptr_free,delptrs
   endif else dprint,dlevel=1,verbose=verbose,'No matching variables to delete'
   return
endif


dt = size(name,/type)
if size(name,/n_dimen) ne 0 then begin
   dprint,verbose=verbose,'Input name must be scalar!'
   error=1
   return
endif

if dt eq 7 then begin
  if total( array_union(byte(name),byte(' *?[]\'))  ge 0) then begin
       dprint,verbose=verbose,'Invalid name: "'+name+'"; Name may not contain spaces, or the characters: "* ? [ ] \"'
       error=1
       return
  endif
  if n_elements(data_quants) eq 0 then index = 0 else $
    index = find_handle(name)
endif else if (dt ge 1) and (dt le 3) then begin
       index = name
       name = data_quants(index).name
    endif else if not keyword_set(delete) then begin
         dprint,verbose=verbose,'Invalid handle name or index'
         error=1
         return
    endif

dq = {tplot_quant}

if n_elements(data_quants) eq 0 then data_quants = [dq]


if index eq 0 then begin        ; new variable
  orig_name = name+'x'          ; required due to compile bug in early versions of IDL
  dq.name = strmid(orig_name,0,strlen(name))
;  if keyword_set(verbose) then print,'Creating new tplot variable: ',dq.name
  verb = 'Creating'
  dq.dh = ptr_new(0)  ;/allocate)
  dq.lh = ptr_new(0)  ;/allocate)
  dq.dl = ptr_new(0)  ;/allocate)
  data_quants = [data_quants,dq]
  index = n_elements(data_quants) - 1
  dq.create_time = systime(1)
endif else begin
  dq = data_quants(index)
;  if keyword_set(verbose) then print,'Replacing tplot variable: ',dq.name
  verb = 'Altering'
endelse


if keyword_set(min) then begin
   bad = where(data.y lt min,c)
   if c ne 0 then data.y(bad) = !values.f_nan
endif

if keyword_set(max) then begin
   bad = where(data.y gt max,c)
   if c ne 0 then data.y(bad) = !values.f_nan
endif


; set values:
if n_elements(newname) ne 0 then begin
    if total( array_union(byte(newname),byte(' *?[]\'))  ge 0) then begin
       dprint,verbose=verbose,dlevel=0,'Invalid name: "'+name+'"; Name may not contain spaces, or the characters: "* ? [ ] \"'
       error=2
       return
    endif
    nindex = where(data_quants.name eq newname, count)
    if count gt 0 then begin
       dprint,verbose=verbose,dlevel=0,'New name must not already be in use!
       error=2
       return
    endif else dq.name = newname
endif

if n_elements(limits) ne 0 then *dq.lh = limits
if n_elements(dlimits) ne 0 then *dq.dl = dlimits
if n_elements(data) ne 0 then begin
    save_ptrs = ptr_extract(except_ptrs)
    save_ptrs = [save_ptrs, ptr_extract(limits) ]
    save_ptrs = [save_ptrs, ptr_extract(dlimits)]
    save_ptrs = [save_ptrs, ptr_extract(data)]
    save_ptrs = [save_ptrs, ptr_extract( data_quants[where(data_quants.name ne dq.name)])]
    dprint,verbose=verbose,dlevel=1,verb+' tplot variable: ',strtrim(index,2),' ',dq.name
    dq.create_time = systime(1)
  if size(/type,data) eq 8 then begin
    mytags = tag_names(data)
    myptrstr = 0
    for i = 0, n_elements(mytags) - 1 do begin
       newv = data.(i)    ;  faster than:   str_element,data,mytags(i),foo
       oldp = ptr_new()
       str_element,*dq.dh,mytags[i],oldp
       if size(/type,newv) ne 10 then begin       ; newv is not a pointer
          ptr_free,ptr_extract(oldp,except=save_ptrs)       ;  free old stuff (if any exist)
          newv = ptr_new(newv,/no_copy)
       endif else begin                           ; newv is a pointer
          if oldp ne newv then  ptr_free,ptr_extract(oldp,except=save_ptrs)
       endelse
       str_element,/add_replace,myptrstr,mytags[i],newv
    endfor
    *dq.dh = myptrstr
  endif else *dq.dh = data
endif

;if n_elements(data) ne 0 then if data_type(data) eq 10 then $
;   dq.dh = data else *dq.dh = data

extract_tags,dstr,data
extract_tags,dstr,dlimits
extract_tags,dstr,limits


str_element,dstr,'x',value=x
str_element,dstr,'y',value=y
if size(/type,x) eq 10 then if ptr_valid(x) then x=*x
if size(/type,y) eq 10 then if ptr_valid(y) then y=*y
if n_elements(x) ne 0 and n_elements(y) ne 0 then begin
    dq.dtype = 1
    dq.trange = minmax(x)
endif
str_element,dstr,'time',value=time
if size(/type,time) eq 10 then time=*time
if n_elements(time) ne 0 then begin                ; obsolete format of passing in an array of structures
    dq.dtype = 2
    message,/info,'Obsolete storage method.  type .cont to continue'
;    stop
;   report
    dq.trange = minmax(time)
        dqtags = tag_names(*dq.dh)
    data_quants(index) = dq
        for i=0,n_elements(dqtags)-1 do if dqtags(i) ne 'TIME' then begin
            subname = dq.name+'.'+dqtags(i)
            str_element,*dq.dh,dqtags(i),foo
            if ndimen(*foo) ne 1 then $
              if dimen((*foo)[0]) ne n_elements(time) then $
                if keyword_set(nostrsw) eq 0 then $
                  *foo = transpose(*foo)
          store_data,subname,data={x: (*dq.dh).time, y:foo}, $
           dlimits=*dq.dl, limits=*dq.lh
        endif
endif

if size(/type,data) eq 7 then begin
    dq.dtype = 3
    names = tnames(data,trange=tr)
    dprint,verbose=verbose,dlevel=1,'Multi-'+' tplot variable: ',strtrim(index,2),' ',dq.name,' : ' ,names
    dq.trange = minmax(tr)
endif

data_quants[index] = dq

;pos = strpos(name,'.')
;if (pos gt 0) then begin
;   names = strarr(2)
;        names(0) = strmid(name,0,pos)
;        names(1) = strmid(name,pos+1,100)
;   superind = find_handle(names(0))
;   dq.trange = data_quants(superind).trange
;endif

end