pro plot_iug_smart, vn, col, row, start_time=st 
  
  ;Check arguments
  npar = n_params()
  if npar lt 1 then return
  if strlen(tnames(vn[0])) eq 0 then return
  if npar eq 1 then begin & col=1 & row=1 & endif
  
  ;Window size
  xsize = 800L & ysize = 800L
  window, 0, xs=xsize, ys=ysize
  erase
  
  ;currently col and row should be equal to or less than 10 
  col = ( col < 10 ) 
  row = ( row < 10 )
  
  ;Obtain the data variable
  get_data, vn[0], data=d
  smt_time = d.x
  smt_dat = d.y 
  dat_idx_max = n_elements(smt_dat[*,0,0])-1
  
  ;Skip to the specified start time
  ;
  
  ;
  dxsize = fix( float(xsize) / col )
  dysize = fix( float(ysize) / row )
  
  img_idx = 0L
  
  for j=0, row-1 do begin
  for i=0, col-1 do begin
    
    if img_idx gt dat_idx_max then break
    
    img = reform( smt_dat[img_idx,*,*] )
    tstr = time_string( smt_time[img_idx], tfor='hh:mm:ss')
    
    ;Origin of the image
    x0 = i * dxsize
    y0 = ysize - (j+1)*dysize
    
    ;Draw the image
    redimg = congrid(img,dxsize,dysize)
    tvscl, redimg, x0, y0, /device
    
    ;Annotate the time label
    xyouts, x0+20,y0+5, tstr, /device, charsize=1.5, color=255
    
        
    img_idx ++
    
  endfor
  endfor
  
  
  
  
  
  
  return
end
