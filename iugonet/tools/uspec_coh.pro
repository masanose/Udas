;+
;NAME:
;uspec_coh
;
;PURPOSE:
;  Calculate the coherence and phase difference between two time-serise data. 
;
;CALLING SEQUENCE:
; uspec_coh, vname1,vname2
; 
;INPUT:
; vname1 = first tplot variable name
; vname2 = second tplot variable name
;
;OUTPUT:
; main_period = the period given the maximum coherence.
; 
;KEYWORDS:
;  deltat  = time interval of observation data
;  width = the width of window function
;    Default is 10.
;  window = type of window function (hanning, boxcar, gaussian, triangle)
;    Default is 'hanning' window.
;  xsize = plot size of x direction. Default is 1400. 
;  ysize = plot size of y direction. Default is 700.
;  
;EXAMPLE:
;   uspec_coh, tplot1, tplot2
;
;CODE:
;R. Hamaguchi, 15/01/2013.
;
;MODIFICATIONS:
;A. Shinbori, 31/01/2013.
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro uspec_coh,vname1,vname2,$
    DELTAT=deltat, $
    WIDTH=width, WINDOW=window, wd=wd,$
    xsize=xsize,ysize=ysize,$
    AMPLITUDE=amplitude, PHASE=phase, $
    FREQ=freq,$
    sl=sl,$
    anomaly_flag=anomaly_flag,$
    main_period

;Get data from two tplot variables:
get_data,vname1,data=d1
get_data,vname2,data=d2

Y1=d1.y  
Y2=d2.y

;Keyword check:   
if not keyword_set(width) then width = 10.0
if not keyword_set(sl) then sl = 0.05
if not keyword_set(wd) then wd = 1
if not keyword_set(xsize) then xsize = 1400
if not keyword_set(ysize) then ysize = 700
if not keyword_set(window) then window = 'hanning';'boxcar', 'gaussian', 'triangle'

;Calculation of cross spectrum. coherence and phase difference:
result = cross_spec( Y1, Y2,DELTAT=deltat, amplitude=amplitude, phase=phase, freq=freq,WIDTH=width,WINDOW=window)

;
g2=1-(sl)^(1.0/((2.0*(n_elements(Y1)-width+1)+1)-1))
print,'coherence confidence interval',g2

max_cxy=0
for i=0,n_elements(result.cxy)-1 do begin
   if finite(result.cxy(i)) then begin 
      if result.cxy(i) ge max_cxy then begin 
         max_cxy=result.cxy(i)
      endif
   endif
endfor

print,'max coherence',max_cxy
main_period=1/(deltat*result.f(where(result.cxy eq max_cxy)))
print,'main_period  [',deltat,'second]',main_period
for i=0,n_elements(result.x)-1 do begin
  if result.cxy(i) gt 0.7 then begin
 ;   print,'coh',result.cxy(i),'     period',1/(deltat*result.f(i)),'     phase',result.lag(i)*180/!pi
  endif
endfor

;Plot of spectra and coherence of two data sets: 
if ~keyword_set(anomaly_flag) then begin
  window, wd, xsize=1400, ysize=700
  !P.Multi = [0, 2, 2, 0, 1]
endif else begin
  window, wd, xsize=1400, ysize=700
  !P.Multi = [0, 2, 2, 0, 1]
endelse
  plot, 1/result.f*deltat,result.x,xtitle = 'Period',ytitle = 'Power spectrum-1',xticklen = 0.5,xgridstyle = 1,$
        /ylog,/xlog,yticklen = 0.5,ygridstyle = 1,yrange=[min(result.x,/NAN),max(result.x,/NAN)];/xlog;,xrange=[1e-3,1e0];,xtitle = 'Frequency(year)',ytitle = 'Power spectrum-1',/ylog,/xlog
  plot, 1/result.f*deltat,result.y,xtitle = 'Period',ytitle = 'Power spectrum-2',xticklen = 0.5,xgridstyle = 1,$
        /ylog,/xlog,yticklen = 0.5,ygridstyle = 1,yrange=[min(result.y,/NAN),max(result.y,/NAN)];xrange=[10,20];/xlog;,xtitle = 'Frequency(year)',ytitle = 'Power spectrum-2',/ylog;,xrange=[0,1.2];,/xlog;,xrange=[10e-9,10e-6]
  ;plot, 1/result.f/deltat,result.absxy,xticklen = 0.5,xgridstyle = 1,/ylog,/xlog;,xtitle = 'Frequency(year)',ytitle = 'Cross spectrum',/ylog;,xrange=[0,1.2];,/xlog;,xrange=[10e-9,10e-6]
  ;plot, 1/result.f/deltat,result.absxy,xticklen = 0.5,xgridstyle = 1,/ylog,/xlog;xrange=[10,20];/xlog
  plot, 1/result.f*deltat,result.cxy,xtitle = 'Period',ytitle = 'Coherence',xticklen = 0.5,xgridstyle = 1,yrange=[0,1],$
        yticklen = 0.5,ygridstyle = 1,/xlog;xrange=[10,20];/xlog;,xtitle = 'Frequency(year)',ytitle = 'Coherence';,xrange=[10e-9,10e-6]
  plot, 1/result.f*deltat,result.lag*180/!pi,xtitle = 'Period',ytitle = 'Phase',xticklen = 0.5,xgridstyle = 1,/xlog,$
        yticks = 5, ytickv = [-180,-90,0,90,180],yrange=[-180,180],yticklen = 0.5,ygridstyle = 1

  end