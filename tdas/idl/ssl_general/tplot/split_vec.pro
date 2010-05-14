PRO split_vec,names,polar=polar,titles=titles, names_out = names_out,suffix=dsuffix,  $
    tensor_suffix=tensor_suffix,vector_suffix=vector_suffix
;+
;NAME:                  split_vec
;PURPOSE:
;                       take a stored vector like 'Vp' and create stored vectors 'Vp_x','Vp_y','Vp_z'
;CALLING SEQUENCE:      split_vec,names
;INPUTS:                names: string or strarr, elements are data handle names
;OPTIONAL INPUTS:
;KEYWORD PARAMETERS:    polar: Use '_mag', '_th', and '_phi'
;			titles: an array of titles to use instead of
;			the default or polar sufficies (NOT USED)
;                       names_out: The variable names of the new variables
;OUTPUTS:
;OPTIONAL OUTPUTS:
;COMMON BLOCKS:
;SIDE EFFECTS:
;RESTRICTIONS:
;EXAMPLE:
;LAST MODIFICATION:     12-mar-2007, jmm
;CREATED BY:            Frank V. Marcoline
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-10-01 10:12:20 -0700 (Thu, 01 Oct 2009) $
;$LastChangedRevision: 6793 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/tplot/split_vec.pro $
;-

  nms = tnames(names)

  num = n_elements(nms)

  if not keyword_set(dsuffix) then begin
    IF keyword_set(polar) THEN dsuffix3 = ['_mag','_th','_phi'] $
    ELSE dsuffix3 = ['_x','_y','_z']
    dsuffix6 = '_'+strsplit('xx yy zz xy xz yz',/extract)
  endif

  names_out = ''
  FOR i=0,num-1 DO BEGIN
    get_data,nms[i],dat=dat,dlim=dlim,lim=lim,alim=alim
      tags = tag_names(dat)
      nt = n_elements(tags)
      ytitle = nms[i]
      str_element,alim,'ytitle',ytitle
      labels = ''
      str_element,alim,'labels',labels
      ny = dimen2(dat.y)

      if keyword_set(dsuffix) then suffix = dsuffix else $
      if ny eq 3  then suffix= dsuffix3  $
      else if ny eq 6 and keyword_set(tensor_suffix) then suffix = dsuffix6 $
      else suffix = strcompress('_'+string(indgen(ny)),/remove)
      for j = 0,ny-1 do begin
         vname = nms[i]+suffix[j]
         store_data,vname,data={x:dat.x,y:dat.y[*,j]},dlim=dlim,lim=lim
;         if keyword_set(labels) then options,vname,/def,ytitle=ytitle+' '+labels[j]

         if is_struct(dlim) then begin
           if in_set(strlowcase(tag_names(dlim)),'labels') && n_elements(dlim.labels) eq ny then begin
             options,vname,/def,labels=dlim.labels[j]
           endif
         
           if in_set(strlowcase(tag_names(dlim)),'colors') && n_elements(get_colors(dlim.colors)) eq ny then begin
             options,vname,/def,colors=(get_colors(dlim.colors))[j]
           endif
         endif
         
         if is_struct(lim) then begin
         
           if in_set(strlowcase(tag_names(lim)),'labels') && n_elements(lim.labels) eq ny then begin
             options,vname,labels=lim.labels[j]
           endif
         
           if in_set(strlowcase(tag_names(lim)),'colors') && n_elements(get_colors(lim.colors)) eq ny then begin
             options,vname,colors=(get_colors(lim.colors))[j]
           endif
           
         endif

         options,vname,/def,ynozero=1
         names_out = [names_out, vname]
      endfor
   ENDFOR
   If(n_elements(names_out) Gt 1) Then names_out = names_out[1:*]
END

