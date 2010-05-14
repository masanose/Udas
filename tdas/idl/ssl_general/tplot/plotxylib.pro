;+
;Procedure: plotxylib
;
;Purpose:  A library of helper functions for plotxy, plotxyz, and plotxyvec.  
;          To make the library available for a routine just call: plotxylib
;          That will force all the routines to be compiled.
;          
;          pxy_set_state sets the state of !TPLOTXY which keeps track
;          of windowing information for the plotxy* routines.  pxy_push_state
;          pushes arguments to these routines onto a data structure that allows
;          them to be replot without retyping the arguments.  pxy_replot will
;          replot a series of calls from memory.  pxy_get_pos will calculate
;          the position and shape of a window from the windowing information
;          in !TPLOTXY and the requested data ranges and margins for a window.
;          pxy_set_window is a routine that houses some redundant
;          initialization code. 
;          
;          SEE ALSO: plotxy,plotxyz,plotxyvec
;
; $LastChangedBy: pcruce $
; $LastChangedDate: 2009-12-15 14:49:37 -0800 (Tue, 15 Dec 2009) $
; $LastChangedRevision: 7011 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/tplot/plotxylib.pro $
;
;-


;HELPER FUNCTION
;set window up, store windowing information
pro pxy_set_window,overplot,addpanel,replot,window,xsize,ysize,wtitle,multi,noisotropic,isotropic=isotropic

  compile_opt hidden,idl2

;if the window has not yet been generated or no more
;windows are available

  if ~keyword_set(overplot) && ~keyword_set(addpanel) && strlowcase(!D.name) ne 'ps' && strlowcase(!D.name) ne 'z' then begin
   
     if ~undefined(window) or keyword_set(xsize) or keyword_set(ysize) or keyword_set(wtitle) or !D.window eq -1 then begin

        device,window_state=wlist
        
        if ~undefined(window) then begin

          ;if the window doesn't really exist create it
           if window ge n_elements(wlist) || window lt 0 then $
              message,'You passed an out of range window value' $
           else if wlist[window] eq 0 then $
              window,window,xsize=640,ysize=512 $
           else $
              wset,window 
    
        endif else if !D.window eq -1 then begin
           window = 0
           xsize = 640
           ysize = 512
        endif else $
           window = !D.window

        if not keyword_set(xsize) then begin
           xs = !D.X_SIZE
        endif else begin
           xs = xsize
        endelse

        if not keyword_set(ysize) then begin
           ys = !D.Y_SIZE
        endif else begin
           ys = ysize
        endelse

        if not keyword_set(wtitle) then begin 
           wt = strcompress('IDL ' + string(window))
        endif else begin
           wt = wtitle
        endelse

        window,window,xsize=xs,ysize=ys,title=wt,retain=2
  
     endif 

   ;if this isn't a recursive call making use of the state information
   ;then we can safely reset the state information
     if ~keyword_set(replot) then begin
      
        pxy_set_state,multi

     endif

  endif

  if keyword_set(addpanel) then begin

     !tplotxy.current++
  
  endif

  if ~keyword_set(noisotropic) then begin
     isotropic = 1
  endif else begin
     isotropic = 0
  endelse

end

;helper function, intializes the tplotxy system variable
pro pxy_set_state,multi

  compile_opt idl2,hidden

  plotlist = 0

  if keyword_set(multi) then begin

     args = strsplit(multi,' *[ ,/:;\.\\] *',/extract,/fold_case,/regex,count=c)

     if c ne 2 then begin
        message,'illegal multi string "' + multi + '"'
     endif

     if stregex(args[0],'r',/boolean) then begin
        revcols = 1
     endif else begin
        revcols = 0
     endelse

     if stregex(args[1],'r',/boolean) then begin
        revrows = 1
     endif else begin
        revrows = 0
     endelse

     cols = long(stregex(args[0],'[0-9]*',/extract))

     if cols eq 0 then begin
        message,'error parsing multi cols: "' + args[0] + '"'
     endif 

     rows = long(stregex(args[1],'[0-9]*',/extract))
     
     if cols eq 0 then begin
        message,'error parsing multi rows: "' + args[1] + '"'
     endif 

  endif else begin
    
     revcols = 0
     revrows = 0
     cols = 1
     rows = 1
 
  endelse

  DEFSYSV,'!tplotxy',exists=bool

  ;free any old memory
  if bool && is_struct(*(!tplotxy.plotvec)) then begin
     
     plotvec = !tplotxy.plotvec

     t=csvector(*plotvec,/free)
     ptr_free,plotvec

  endif

  tplotxyval = { rows:rows,$
                 revrows:revrows,$
                 cols:cols,$
                 revcols:revcols,$
                 current:0,$
                 pos:dblarr(4),$ ;these values needed to properly overplot arrows
                 xrange:dblarr(2),$
                 yrange:dblarr(2),$
                 plotvec:ptr_new(csvector('start')) }


  DEFSYSV,'!tplotxy',tplotxyval

end


;adds the information to repeat the previous tplotxy call to the 
;tplotxy global variable
pro pxy_push_state,func_name,state,_extra=ex

  compile_opt idl2,hidden

  plotvec = *(!tplotxy.plotvec)

  ptr_free,!tplotxy.plotvec

  str_element,state,'ex',ex,/add

  func = {func:func_name,state:state}

  !tplotxy.plotvec = ptr_new(csvector(func,plotvec))

end

;determines the position of the current panel
;using the margin information and the tplotxy information
;assumes data is already logarithm'd, sorted, etc...

function pxy_get_pos,x,y,isotropic,xmargin,ymargin

  compile_opt idl2,hidden

  ;validate inputs
  if keyword_set(xmargin) then begin

     if n_elements(xmargin) ne 2 then begin
        message,'malformed x margin'
     endif

     if ~is_num(xmargin,/real) then begin
        message,'x margin must contain real number'
     endif

     id = where(xmargin gt 1 or xmargin lt 0) 

     if id[0] ne -1 then begin
        message,'x margin must be in the range [0,1]'
     endif

  endif else begin

     xmargin = [.15,.17]

  endelse
              
  if keyword_set(ymargin) then begin

     if n_elements(ymargin) ne 2 then begin
        message,'malformed y margin'
     endif
     
     if ~is_num(ymargin,/real) then begin
        message,'y margin must contain real number'
     endif

     id = where(ymargin gt 1 or ymargin lt 0) 

     if id[0] ne -1 then begin
        message,'y margin must be in the range [0,1]'
     endif

  endif else begin

     ymargin = [.1,.075]

  endelse

  ;verify that the current panel is not too large
  
  if !tplotxy.current ge (!tplotxy.rows * !tplotxy.cols) then begin
     message,'no more panels available in current layout'
  endif

  xpanel_size = 1.0/!tplotxy.cols
  ypanel_size = 1.0/!tplotxy.rows

  if !tplotxy.revcols eq 0 then begin
     xpanel_cur = !tplotxy.current mod !tplotxy.cols
  endif else begin
     xpanel_cur = !tplotxy.cols - 1 - (!tplotxy.current mod !tplotxy.cols)
  endelse

  if !tplotxy.revrows ne 0 then begin
     ypanel_cur = !tplotxy.current / !tplotxy.cols
  endif else begin
     ypanel_cur = !tplotxy.rows - 1 - (!tplotxy.current / !tplotxy.cols)
  endelse

  ;coordinates
  x1 = xpanel_cur * xpanel_size + xpanel_size*xmargin[0]
  x2 = (xpanel_cur+1) * xpanel_size - xpanel_size*xmargin[1]

  y1 = ypanel_cur * ypanel_size + ypanel_size*ymargin[0]
  y2 = (ypanel_cur+1) * ypanel_size - ypanel_size*ymargin[1]

  if keyword_set(isotropic) then begin

     x_data_sz = abs(double(x[1])-double(x[0]))

     y_data_sz = abs(double(y[1])-double(y[0]))

     ;plot size normalized into centimeters for comparisons
     x_plot_sz = !D.x_size*(x2-x1)/!D.x_px_cm
     
     y_plot_sz = !D.y_size*(y2-y1)/!D.y_px_cm

     if x_data_sz/y_data_sz lt x_plot_sz/y_plot_sz then begin

        x_plot_sz = y_plot_sz * x_data_sz/y_data_sz

     endif else begin

        y_plot_sz = x_plot_sz * y_data_sz/x_data_sz

     endelse 
     
     x2 = x1 + !D.x_px_cm * x_plot_sz/!D.x_size

     y2 = y1 + !D.y_px_cm * y_plot_sz/!D.y_size 
       
  endif
  
  !tplotxy.pos = [x1,y1,x2,y2]
  !tplotxy.xrange = x
  !tplotxy.yrange = y

  return, [x1,y1,x2,y2]

end

pro pxy_replot

  compile_opt idl2,hidden

  !tplotxy.current = 0

  plotvec = *(!tplotxy.plotvec)

  len = csvector(plotvec,/length)

  for i = 1,len-1 do begin

     c = csvector(i,plotvec,/read)

     state=c.state

     if c.func eq 'plotxy' then begin

        plotxy, state.vectors,replot=1,_extra=state.ex

     endif else if c.func eq 'plotxyz' then begin

        plotxyz,state.x,state.y,state.z,replot=1,_extra=state.ex

     endif else if c.func eq 'plotxyvec' then begin

        plotxyvec,state.xy,state.dxy,replot=1,_extra=state.ex
        
     endif else begin
        
        message,'unrecognized replot function'

     endelse

  endfor

end

pro plotxylib

;does nothing
;call plotxylib at the beginning of any
;routine that needs the routines in this
;library to guarantee that they are compiled

end
