;+
;name
; normality_test
;purpose
; カイ二乗適合度検定を用いて、正規分布に従っているか検定する
;syntax
; result=normality_test(x,sl=sl,mv=mv)
;Keywords:
;  x:検定を行う観測データ
;  sl:有意水準
;     入力しなければ５％で検定
;  mv:欠損値
;     入力しなければNaNのみを欠損値として処理;
;結果は、正規分布に従っている場合は0、従っていない場合は1を返す
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

function normality_test,x,sl=sl,mv=mv

nx=n_elements(x)

if keyword_set(mv) then begin
    for i=0L,nx-1 do begin
        if(float(x[i]) eq mv) or (not finite(x[i])) then continue
        append_array,c,float(x[i])
    endfor
endif else begin
    for i=0L,nx-1 do begin
        if (not finite(x[i])) then continue
        append_array,c,float(x[i])
    endfor
endelse

nc=n_elements(c)
x_max=max(c)
x_min=min(c)
x_mean=mean(c)
x_stddev=stddev(c)

if nc ge 400 then begin
    nK=round(nc/40.0)             ;区間の数は（データ点数÷40） データ点数が400点未満であれば、区間は10固定
endif else begin
    nK=10.0
endelse
x_d=(x_max-x_min)/nK

for j=0L,nK-2 do begin
    r1=where(c ge (x_min+x_d*j) and c lt (x_min+x_d*(j+1)))
    if r1(0) ne -1 then begin
        append_array,y,n_elements(r1)             ;実測度数
    endif else begin
        append_array,y,0
    endelse
    append_array,z1,x_min+x_d*j
endfor

rx=where(c ge (x_min+x_d*(nK-1)) and c le (x_max))
append_array,y,n_elements(rx)
append_array,z1,x_min+x_d*(nK-1)
append_array,z1,x_min+x_d*nK

for i=0L,nK do begin
    append_array,z2,gaussint((z1[i]-x_mean)/x_stddev)
endfor


for i=0L,nK-1 do begin
    append_array,z3,(z2[i+1]-z2[i])*nc          ;期待度数
endfor

if nK ge 15 then begin                       ;区間が15以上は誤差の大きい両端をカット
    for i=round(nK/20.0),nK-round(nK/20.0)-1 do begin           
        append_array,z4,((y[i]-z3[i])^2.0)/z3[i]
    endfor
endif else begin
    for i=0L,nK-1 do begin           
        append_array,z4,((y[i]-z3[i])^2.0)/z3[i]
    endfor
endelse

;print,total(z3),total(y)
sum_z4=total(z4)

;print,z2,z3,y
;print,total(z3),total(y)
window, 2, xsize=850, ysize=410
        !P.MULTI = [0,1,1]
plot,y
oplot,z3
        
if keyword_set(sl) then begin 
    v=chisqr_cvf(sl,nK-1-2)
    if (sum_z4 le v) then begin
    char='   NORMAL DISTRIBUTION with significance level ='
    result=0
    endif else begin
    char='   NOT NORMAL DISTRIBUTION with significance level ='
    result=1
    endelse
    print,'comment:',char,sl
endif else begin
    v=chisqr_cvf(0.05,nK-1-2)   
    if (sum_z4 le v) then begin
    char='   NORMAL DISTRIBUTION with significance level = 0.05'
    result=0
    endif else begin
    char='   NOT NORMAL DISTRIBUTION with significance level = 0.05'
    result=1
    endelse
    print,'comment:',char
endelse

return,result
end