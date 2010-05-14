;+
;Processes tags from settings from dl and applies them to an axis 
;
;Inputs:
;  axis(object ref): thm_ui_axis_settings
;  axis(string):  string naming the axis, can be 'x','y',or 'z'
;  dlptr(ptr to struct):  The dlimits pointer from the data object
;  lptr(ptr to struct): The limits pointer from the data object 
;                
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-05-12 12:07:37 -0700 (Tue, 12 May 2009) $
;$LastChangedRevision: 5831 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/panels/thm_ui_layout_options/thm_ui_process_axis_tags.pro $
;-


pro thm_ui_process_axis_tags,axis,prefix,dlptr,lptr

  compile_opt idl2,hidden
 
  if ptr_valid(dlptr) && ptr_valid(lptr) then begin
    ;must dereference first to prevent permanent mutation of dl/l
    dl = *dlptr
    l = *lptr
    extract_tags,dl,l
  endif else if ptr_valid(dlptr) then begin
    dl = *dlptr
  endif else if ptr_valid(lptr) then begin
    dl = *lptr
  endif else begin
    return
  endelse

  ;invalid struct means we stop here
  if ~(size(dl,/type) eq 8) then return

  ; set axis scaling according to limits/dlimits
  str_element,dl,prefix+'log',log,success=s
  if s then begin
    axis->setProperty, scaling=log
  endif; else yAxis->setProperty, scaling=0
 
  ; set axis limits and turn on fixed yrange
  str_element,dl,prefix+'range',range,success=s
  
  if s then begin
    if n_elements(range) eq 2 && $
     range[0] ne range[1] then begin ; tplot uses 0 range as a signal that this range setting should be ignored
       
       if prefix eq 'z' then begin
         axis->getProperty,$
           minRange=minFixedRange,$
           maxRange=maxFixedRange
       endif else begin
         axis->getproperty, $
            minfixedrange=minfixedrange,$
            maxfixedrange=maxfixedrange
       endelse
       
       if (minfixedrange eq 0) && (maxfixedrange eq 0) then begin
         minfixedrange = range[0]
         maxfixedrange = range[1]
       endif
       
       minfixedrange = min([minfixedrange,range[0]],/nan)
       maxfixedrange = max([maxfixedrange,range[1]],/nan)
       
       if prefix eq 'z' then begin
         Axis->setProperty, $
              minrange=minfixedrange,$
              maxrange=maxfixedrange, $
              fixed=1
       endif else begin  
         Axis->setProperty, $
              minfixedrange=minfixedrange,$
              maxfixedrange=maxfixedrange, $
              rangeoption=2
       endelse
    endif
  endif
  
  str_element,dl,prefix+'style',style,success=s
  
  if s && prefix ne 'z' then begin
    axis->setProperty,rangemargin=0
  endif
  
  ;  ;set labels
;  if in_set('labels', strlowcase(tag_names(dl))) then begin
;    if ntraces gt n_elements(dl.labels) then ylabel=yvar $
;      else ylabel=dl.labels[ntraces]
;  endif
 
end