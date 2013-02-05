;y1 = sin( 2*!pi * findgen( 10000 ) / 23. ) + sin( 4*!pi * findgen( 10000 ) / 23. ) + 1 * randomn( 1, 10000 )
;y2 = sin( 2*!pi * ( findgen( 10000 ) / 23. - 0.25 ) ) + sin( 1*!pi * findgen( 10000 ) / 23. ) + 1 * randomn( 2, 10000 )
fs = 100.0;                          % Sample frequency (Hz)
t = findgen( 1000 )/fs;                % 10 sec sample
y1 = (1.3)*sin(2*!pi*15*t)  + (1.7)*sin(2*!pi*40*(t-2))   + (2.5)*randomn(seed,n_elements(t)); randomn(seed,num_point)

y2 =  sin(2*!pi*15*t+!pi/2.0)  + (0.7)*sin(2*!pi*9*(t-2))  +0.5*t + (0.5)*randomn(seed,n_elements(t)); 


;Definition of filtering width and window function:
width = 20
window = 'hanning';'boxcar', 'gaussian', 'triangle'

sl=0.01
g2=1-(sl)^(1/(float(width)-1))
print,'g2=',g2
for i=0,n_elements(Y1)-1 do begin
  append_array,g2line,g2
endfor
result = cross_spec( y1, y2, amplitude=amplitude, phase=phase, freq=freq,WIDTH=width,WINDOW=window,deltat=1/fs)
  window, 0, xsize=1000, ysize=510
  !P.Multi = [0, 2, 2, 0, 1]
  plot, result.f,result.x,xtitle = 'Frequency',ytitle = 'Power spectrum-1',xticks = 10,xticklen = 0.5,xgridstyle = 1
  plot, result.f,result.y,xtitle = 'Frequency',ytitle = 'Power spectrum-2',xticks = 10,xticklen = 0.5,xgridstyle = 1
  ;plot, result.f,result.absxy,xtitle = 'Frequency',ytitle = 'Cross spectrum',xticks = 10,xticklen = 0.5,xgridstyle = 1
  plot, result.f,result.cxy^2,xtitle = 'Frequency',ytitle = 'Coherence',xticks = 10,xticklen = 0.5,xgridstyle = 1
  oplot,result.f,g2line,color=230
  plot, result.f,result.lag*180.0/!pi,xtitle = 'Frequency',ytitle = 'Phase',xticks = 10,yticks = 5, ytickv = [-180,-90,0,90,180],yrange=[-180,180],yticklen = 0.5,ygridstyle = 1,xticklen = 0.5,xgridstyle = 1

  window, 1, xsize=500, ysize=510
  !P.Multi = [0, 1, 2, 0, 1]
  plot,y1
  plot,y2
;  thisDevice=!D.NAME 
;  set_plot,'ps'
;  !p.font = 0
;  device, /helvetica, /bold
; ; device,ysize=10
;  device,filename='test_result.ps'
;  !P.Multi = [0, 2, 2, 0, 1]
;  plot, result.f,result.x,xtitle = 'Frequency',ytitle = 'Power spectrum-1'
;  plot, result.f,result.y,xtitle = 'Frequency',ytitle = 'Power spectrum-2'
;  plot, result.f,result.absxy,xtitle = 'Frequency',ytitle = 'Cross spectrum'
;  plot, result.f,result.cxy,xtitle = 'Frequency',ytitle = 'Coherence' ;
;
;  device,/color
;;  device,/close
;  set_plot,thisDevice
end