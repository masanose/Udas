;+
;NAME:
;ucross_cor
;
;PURPOSE:
;  Calculate the cross correlation between two time-serise data 
;  and perform a test of correlation. 
;
;CALLING SEQUENCE:
; ucross_cor, vname1,vname2
; 
;INPUT:
; vname1 = first tplot variable name
; vname2 = second tplot variable name
;
;OUTPUT:
; cross_cor = cross correlation between two time-serise data
; x_cor = auto correlation of the first tplot variable
; y_cor = auto correlation of the second tplot variable
; 
;KEYWORDS:
;  sl  = Significant level of correlation test.
;  low_lag = lag time of left direction
;  high_lag = lag time of right direction
;  
;EXAMPLE:
;   ucross_cor, tplot1, tplot2
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

pro ucross_cor,vname1,vname2, cross_cor,$
    sl=sl,low_lag=low_lag, high_lag=high_lag, x_cor,y_cor

;Get data from two tplot variables:
get_data,vname1,data=d1
get_data,vname2,data=d2

x=d1.y
y=d2.y

;
for i=0,n_elements(x)-1 do begin
   if (finite(x(i))) and (finite(y(i))) then begin
      append_array,x1,double(x(i))
      append_array,y1,double(y(i))
   endif
endfor

;Keyword check of low_lag: =0ではkeyword_setは0を返す。
if ~keyword_set(low_lag) then begin
   low_lag=-n_elements(x1)/2
endif

;Keyword check of high_lag:
if ~keyword_set(high_lag) then begin
   high_lag=n_elements(x1)/2
endif

cross_cor=dblarr(high_lag-low_lag+1)
x_cor=dblarr(high_lag-low_lag+1)
y_cor=dblarr(high_lag-low_lag+1)

for i=low_lag,high_lag do begin
   cross_cor(i-low_lag)=C_CORRELATE(x1,y1,i)
   x_cor(i-low_lag)=A_CORRELATE(x1,i)
   y_cor(i-low_lag)=A_CORRELATE(y1,i)
endfor

;Calculate statistical test values:
max_corr=max(cross_cor)
max_zure=where(cross_cor eq max(cross_cor))+low_lag
max_stat=[abs(max_corr)]*[sqrt((n_elements(x1)-abs(max_zure)-2)/(1-max_corr^2))]

min_corr=min(cross_cor)
min_zure=where(cross_cor eq min(cross_cor))+low_lag
min_stat=[abs(min_corr)]*[sqrt((n_elements(x1)-abs(min_zure)-2)/(1-min_corr^2))]

;Calculate a t-distribution value for selected significant level
if ~keyword_set(sl) then begin
    sl=0.05
endif
result=t_cvf(sl/2.0,n_elements(x1)-2)

append_array,cor,max_corr
append_array,cor,max_zure
append_array,cor,max_stat

;If the lag value is positive, it is shown that the second data are delayed:
print,'-----------------cross correlation status--------------------------'
print,'|        maximun correlation coefficient       =',max_corr
print,'|               lag of max correlation         =',max_zure
if (max_stat ge result) then begin
print,'|statistically significant (significance Lv    =',sl,')'
endif else begin
print,'|NOT statistically significant (significance Lv    =',sl,')'
;print,'|There is statistically no correlation between two data sets on this lag case.(significance Lv    =',sl,')'
endelse
print,'|        minimun correlation coefficient       =',min_corr
print,'|               lag of min correlation         =',min_zure
if (min_stat ge result) then begin
print,'|statistically significant (significance Lv    =',sl,')'
endif else begin
print,'|NOT statistically significant (significance Lv    =',sl,')'
endelse

print,'-------------------------------------------------------------------'

end 