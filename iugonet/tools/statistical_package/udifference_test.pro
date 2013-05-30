;+
;NAME:
;　　udifference_test
;
;PURPOSE:
;  平均値検定を行うプログラム。
;  χ二乗検定によって正規分布との適合を検定する。
;  どちらも正規分布に従う場合はWelch検定
;  どちらかが正規分布に従わない場合はマンホイットニー検定のみを用いる。
;  
;SYNTAX:
;  difference_test,vname1,vname2,sl=**
;  
;KEYWORDS:
;  result:検定結果を'0'：ウェルチ検定使用-判定は異,'1'：ウェルチ検定使用-判定は同,'2'：マンホイットニー-異,'3'：マンホイットニー-同、で返す
;  sl:有意水準。指定しない場合はsl=0.05で検定。
;  test_sel：行う検定を指定。'2'はマンホイットニー検定、'1'はウェルチ検定、'0'はχ二乗検定によって正規分布との適合を検定
;  
;CODE:
;R. Hamaguchi, 13/02/2012.
;
;MODIFICATIONS:
;A. Shinbori, 01/05/2013.
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro udifference_test,vname1,vname2,result,sl=sl,test_sel=test_sel

;***********************
;Keyword check test_sel:
;***********************
if not keyword_set(test_sel) then test_sel=0 

;Get data from two tplot variables:
if strlen(tnames(vname1)) * strlen(tnames(vname2)) eq 0 then begin
  print, 'Cannot find the tplot vars in argument!'
  return
endif
get_data,vname1,data=d1
get_data,vname2,data=d2

x=d1.y
y=d2.y

;Welch or Mann Whitney test:
if test_sel eq 1 then begin
   result=welch_test(x,y,sl=sl)
endif 
if test_sel eq 2 then begin
   result=mann_whitney_test(x,y,sl=sl)
endif

if test_sel eq 0 then begin
   x1=normality_test(x,sl=sl)
   y1=normality_test(y,sl=sl)
   if (x1 eq 0) and (y1 eq 0) then begin 
      result=welch_test(x,y,sl=sl)
   endif else begin
      result=mann_whitney_test(x,y,sl=sl)
   endelse
endif
;The end:    
end