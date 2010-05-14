function spinmodel_findseg_t,mptr,t
sp = (*mptr).segs_ptr
currseg = (*sp)[(*mptr).index_t]
if ( (currseg.t1 LE t) AND (t LE currseg.t2) ) then begin
  return, (*mptr).index_t
endif else if (t LE (*sp)[0].t1) then begin
  (*mptr).index_t = 0
  return, (*mptr).index_t
endif else if (t GE (*sp)[(*mptr).lastseg].t2) then begin
  (*mptr).index_t = (*mptr).lastseg
  return, (*mptr).index_t
endif else if (t LE currseg.t1) then begin
  start_index = 0
endif else start_index = (*mptr).index_t + 1

idx = where((*sp).t1 le t and t le (*sp).t2,c)
if c ne 1 then begin
  ;some error goes here
  return,-1 ;fail code
endif else begin
  (*mptr).index_t = idx
  return,(*mptr).index_t
endelse

;for i=start_index,(*mptr).lastseg,1 do begin
;  currseg = (*sp)[i]
;  if ((currseg.t1 LE t) AND (t LE currseg.t2)) then begin
;     (*mptr).index_t = i
;     return, (*mptr).index_t
;  endif 
;endfor
end
