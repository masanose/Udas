




function iug_load_gmag_wdc_relpath_to_year, relpath, sname
  year_str = 0
  segm = strsplit(relpath, "/", /extract)

  if strlowcase(sname) eq 'sym' $
     or strlowcase(sname) eq 'asy' $
     or strlowcase(sname) eq 'ae' $
     or strlowcase(sname) eq 'dst' then begin

     for i = 0l, size(segm,/n_elements) -1 do begin
        if segm[i] eq 'index' then begin
           year_str = segm[i+2]
           break
        endif
     endfor

  endif else begin

     for i = 0l, size(segm,/n_elements) -1 do begin
        if segm[i] eq strlowcase(sname) then begin
           year_str = segm[i+1]
           break
        endif
     endfor

  endelse

  return, year_str
end
