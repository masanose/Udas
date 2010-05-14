function spinmodel_findseg_n,mptr,n
sp = (*mptr).segs_ptr
currseg = (*sp)[(*mptr).index_n]
if ( (currseg.c1 LE n) AND (n LE currseg.c2) ) then begin
  return, (*mptr).index_n
endif else if (n LE (*sp)[0].c1) then begin
  (*mptr).index_n = 0
  return, (*mptr).index_n
endif else if (n GE (*sp)[(*mptr).lastseg].c2) then begin
 (*mptr).index_n = (*mptr).lastseg
  return, (*mptr).index_n
endif else if (n LE currseg.c1) then begin
  start_index = 0
endif else start_index = (*mptr).index_n + 1

idx = where((*sp).c1 le n and n le (*sp).c2,c)
if c ne 1 then begin
  ;some error goes here
  return,-1 ;fail code
endif else begin
  (*mptr).index_n = idx
  return,(*mptr).index_n
endelse

;for i=start_index,(*mptr).lastseg,1 do begin
;  currseg = (*sp)[i]
;  if ((currseg.c1 LE n) AND (n LE currseg.c2))then begin
;     (*mptr).index_n = i
;     return, (*mptr).index_n
;  endif 
;endfor
end
