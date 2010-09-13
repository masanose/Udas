;+
;NAME:
;    trange_clip
;
;PURPOSE:
;    Utility to trim the time range of a tplot variable and remove
;    excess data.
;
; Inputs:
;         name: Either tplot variable name
;                or data structure (if data_in keyword specified)
;           t1: Start time (double, time since 1970)
;           t2: Stop time (double, time since 1970)
;
; Options:
;          data_in: Set to specify that the input is a data structure,
;                     not a tplot name.
;     remove_match: Removes data between t1 and t2
;                     (default is keep only data between t1 and t2)
;
; Outputs:
;    Modifies the specified tplot name or data structure.
;
; MODS:
;    REE. 09-05-11. Changed to work with CLEAN EFP. Added BadClip=BadClip
;-
pro trange_clip,name, t1, t2, newname=newname, data_in=data_in,$
                remove_match=remove_match, BadClip=BadClip

  BadClip = 0
  ; Get data
  if keyword_set(data_in) then $
    data=temporary(name) $
  else get_data,name,data=data,lim=lim,dlim=dlim
  if n_tags(data) eq 0 then return

  ; Find start/stop times in data
  i1=value_locate(data.x,t1)+1
  if (i1 NE 0) then if (data.x(i1-1) EQ t1) then i1 = i1-1
  i2=value_locate(data.x,t2)
  if i2 LT i1 or i2 EQ -1 or i1 GE n_elements(data.x)-1 then begin
    print,'trange_clip WARNING: All data times are outside requested clipping window.'
    print,'                     No data clipping performed.'
    if keyword_set(data_in) then name=temporary(data)
    BadClip = 1
    return
  endif
  i1>=0

  ; Trim data
  case n_tags(data) of
    2: if keyword_set(remove_match) then begin
         if i1 eq 0 then begin
           data={x:data.x[i2:*],y:data.y[i2:*,*]}
         endif else begin
           if i2 ne n_elements(data.x)-1 then begin
             data={x:[data.x[0:i1],data.x[i2:*]],y:[data.y[0:i1,*],data.y[i2:*,*]]}
           endif else data={x:data.x[0:i1],y:data.y[0:i1,*]}
         endelse
       endif else data={x:data.x[i1:i2],y:data.y[i1:i2,*]}
    3:if keyword_set(remove_match) then begin
         if i1 eq 0 then begin
           data={x:data.x[i2:*],y:data.y[i2:*,*],v:data.v}
         endif else begin
           if i2 ne n_elements(data.x)-1 then begin
             data={x:[data.x[0:i1],data.x[i2:*]],y:[data.y[0:i1,*],data.y[i2:*,*]],v:data.v}
           endif else data={x:data.x[0:i1],y:data.y[0:i1,*],v:data.v}
         endelse
       endif else data={x:data.x[i1:i2],y:data.y[i1:i2,*],v:data.v}
    else: begin
            print,'trange_clip ERROR: Unrecognized data type.'
            if keyword_set(data_in) then name=temporary(data)
            BadClip = 1            
            return
          end
  endcase

  ; Store tplot variable
  if keyword_set(data_in) then begin
    name=temporary(data)
  endif else begin
    if n_elements(newname) eq 0 then newname=name
    store_data,newname,data=data,lim=lim,dlim=dlim
  endelse
end
