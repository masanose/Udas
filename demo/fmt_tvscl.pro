; FMT Image Quick Look Program
;
; INPUT ARGUMENT
;  time = yyyymmddhhmmss (string)
; OUTPUT ARGUMENT
;  imgtime = YYYY-MM-DD/hh:mm:ss (string)
;
; Version: 1.1
; History: 2010.05.11 S.UeNo: Completion of Ver.1
;          2010.05.12 S.UeNo: Addition of output argument "imgtime"

pro fmt_tvscl,time,imgtime

;Create a window for plotting
window,6,xs=1392*0.3,ys=1040*0.3*2
loadct,0

ch_img=intarr(1392,1040)
ph_img=intarr(1392,1040)

yyyy=strmid(time,0,4)
mon=strmid(time,4,2)
dd=strmid(time,6,2)
hh=strmid(time,8,2)
min=strmid(time,10,2)
ss=strmid(time,12,2)

julian=julday(mon,dd,yyyy,hh,min,ss)
 julian0=julian
time0=1.*hh*60.*60.+1.*min*60.+1.*ss

;-------------------------------------------------
chromosphere:
;-------------------------------------------------

if (!version.os_family eq 'Windows') then begin
 search_name1='FMT\Daily\'+yyyy+mon+dd+'*\Ha\*ha*'
endif
if (!version.os_family eq 'unix') then begin
 search_name1='FMT/Daily/'+yyyy+mon+dd+'*/Ha/*ha*'
endif

 files1=findfile(search_name1)

 if (files1(0) ne '') then begin
  Nf1=n_elements(files1)
  timearr=fltarr(Nf1)
  for t=0,Nf1-1 do begin
   hh2=strmid(files1(t),30,2)
   min2=strmid(files1(t),32,2)
   ss2=strmid(files1(t),34,2)
   timearr(t)=1.*hh2*60.*60.+1.*min2*60.+1.*ss2
  endfor
  w=where((timearr-time0) le 0.)
   if (w(0) ne -1) then begin
    Nw=n_elements(w)
    If1=w(Nw-1)
   endif else begin
    files1(0)=''
    If1=0
   endelse
  ;print,'File name: ',files1(If1)
 endif 

  julian=julian0
 while ((files1(0) eq '') and (julian ge julian0-30.)) do begin
  ;print,julian
  julian=julian-1.
  caldat,julian,month,day,year
  mon1=string(format='(i2.2)',month)
  dd1=string(format='(i2.2)',day)
  yyyy1=string(format='(i4.4)',year)
  if (!version.os_family eq 'Windows') then begin
  search_name1='FMT\Daily\'+yyyy1+mon1+dd1+'*\Ha\*ha*'
  endif
  if (!version.os_family eq 'unix') then begin
  search_name1='FMT/Daily/'+yyyy1+mon1+dd1+'*/Ha/*ha*'
  endif
  files1=findfile(search_name1)
  Nf1=n_elements(files1)
  If1=Nf1-1
  ;print,'File name: ',files1(If1)
  if (julian eq julian0-30.) then begin
   print,'NO DATA OF CHROMOSPHERE!!'
   goto,photosphere
  endif
 endwhile

;-------------------------------------------------
; Integration of chromospheric images during 10 min

ch_img0=intarr(1392,1040)
ch_img1=intarr(1392,1040)

openr,1,files1(If1),/compress
readu,1,ch_img0
close,1

xprof=smooth(total(ch_img0,2),13)
dxprof=xprof-shift(xprof,1)
wx1=where(dxprof(10:1382) eq max(dxprof(10:1382)))+10
wx2=where(dxprof(10:1382) eq min(dxprof(10:1382)))+10
xcen0=(mean(wx1)+mean(wx2))/2.

yprof=smooth(total(ch_img0,1),13)
dyprof=yprof-shift(yprof,1)
wy1=where(dyprof(10:1030) eq max(dyprof(10:1030)))+10
wy2=where(dyprof(10:1030) eq min(dyprof(10:1030)))+10
ycen0=(mean(wy1)+mean(wy2))/2.

;print,'(Xcen0,Ycen0)=',xcen0,ycen0

if (ycen0 ge 470) and (ycen0 le 570) and (xcen0 ge 656) and (xcen0 le 736) then begin
 ch_img=shift(ch_img0*1.,696-xcen0,520-ycen0)
 maisuu=1.
endif else begin
 ch_img=ch_img0*0.
 maisuu=0.
endelse

 dd5=strmid(files1(If1),28,2)
 hh5=strmid(files1(If1),30,2)
 min5=strmid(files1(If1),32,2)
 ss5=strmid(files1(If1),34,2)
 time_sec0=dd5*24.*60.*60. + hh5*60.*60. + min5*60. + ss5*1.
 time_sec1=time_sec0

j=1
while ((time_sec0 - time_sec1) le 600.) and ((If1-j) ge 0) do begin

 dd6=strmid(files1(If1-j),28,2)
 hh6=strmid(files1(If1-j),30,2)
 min6=strmid(files1(If1-j),32,2)
 ss6=strmid(files1(If1-j),34,2)
 time_sec1=dd6*24.*60.*60. + hh6*60.*60. + min6*60. + ss6*1.

 openr,1,files1(If1-j),/compress
 readu,1,ch_img1
 close,1

 xprof=smooth(total(ch_img1,2),13)
 dxprof=xprof-shift(xprof,1)
 wx1=where(dxprof(10:1382) eq max(dxprof(10:1382)))+10
 wx2=where(dxprof(10:1382) eq min(dxprof(10:1382)))+10
 xcen1=(mean(wx1)+mean(wx2))/2.

 yprof=smooth(total(ch_img1,1),13)
 dyprof=yprof-shift(yprof,1)
 wy1=where(dyprof(10:1030) eq max(dyprof(10:1030)))+10
 wy2=where(dyprof(10:1030) eq min(dyprof(10:1030)))+10
 ycen1=(mean(wy1)+mean(wy2))/2.

; print,'(Xcen1,Ycen1)=',xcen1,ycen1

 if (ycen1 ge 470) and (ycen1 le 570) and (xcen1 ge 656) and (xcen1 le 736) then begin
  ch_img=ch_img + shift(1.*ch_img1,696-xcen1,520-ycen1)
  maisuu=maisuu+1.
 endif
 j=j+1

endwhile

if (maisuu eq 0.) then begin
 print,'NO DATA OF CHROMOSPHERE!!'
 goto,photosphere
endif

ch_img=ch_img/maisuu

fac=1040/512.
a=ch_img(176:1215,*)
a=rotate(a,7)
a=smooth(a,3)
a=1.9*a-smooth(a,11) 
a=bytscl(a) 
b=a(*,253*fac)/5.+a(*,254*fac)/5.+a(*,255*fac)/5.+a(*,256*fac)/5.+a(*,257*fac)/5. 
ww=[0,1,2,3,4,5,506,507,508,509,510,511]*fac 
std=stdev(b(ww),mean2) 
d=a<(max(a)-0)>(mean2-5)  
d=bytscl(d) 
ch_img(*,*)=0
ch_img(176:1215,*)=d(*,*)

;--------------------------------------------------------
; Display of the chromospheric image

ch_img1=1.9*ch_img-smooth(ch_img,9) ; Unsharp-mask
tvscl,congrid(ch_img1,1392*0.3,1040*0.3,/interp),0,1040*0.3*1
yyyy3=strmid(files1(If1),22,4)
mon3=strmid(files1(If1),26,2)
dd3=strmid(files1(If1),28,2)
hh3=strmid(files1(If1),30,2)
min3=strmid(files1(If1),32,2)
ss3=strmid(files1(If1),34,2)
timestamp=yyyy3+'-'+mon3+'-'+dd3+' '+hh3+':'+min3+':'+ss3
imgtime=yyyy3+'-'+mon3+'-'+dd3+'/'+hh3+':'+min3+':'+ss3
;YYYY-MM-DD/hh:mm:ss
xyouts,20*0.6,490*0.6+1040*0.3*1,timestamp,/device,chars=1.2,color=255
xyouts,20*0.6+850*0.3,490*0.6+1040*0.3*1,'Solar Chromosphrere',/device,chars=1.2,color=255
xyouts,20*0.6+930*0.3,460*0.6+1040*0.3*1,'Hida Obs./FMT',/device,chars=1.2,color=255

;-------------------------------------------------
photosphere:
;-------------------------------------------------

 if (!version.os_family eq 'Windows') then begin
 search_name2='FMT\Daily\'+yyyy+mon+dd+'*\Wl\*wl*'
 endif
 if (!version.os_family eq 'unix') then begin
 search_name2='FMT/Daily/'+yyyy+mon+dd+'*/Wl/*wl*'
 endif
 files2=findfile(search_name2)

 if (files2(0) ne '') then begin
  Nf2=n_elements(files2)
  timearr=fltarr(Nf2)
  for t=0,Nf2-1 do begin
   hh2=strmid(files2(t),30,2)
   min2=strmid(files2(t),32,2)
   ss2=strmid(files2(t),34,2)
   timearr(t)=1.*hh2*60.*60.+1.*min2*60.+1.*ss2
  endfor
  w=where((timearr-time0) le 0.)
   if (w(0) ne -1) then begin
    Nw=n_elements(w)
    If2=w(Nw-1)
   endif else begin
    files2(0)=''
    If2=0
   endelse
  ;print,'File name: ',files2(If2)
 endif 

  julian=julian0
 while ((files2(0) eq '') and (julian ge julian0-30.)) do begin
  ;print,julian
  julian=julian-1.
  caldat,julian,month,day,year
  mon2=string(format='(i2.2)',month)
  dd2=string(format='(i2.2)',day)
  yyyy2=string(format='(i4.4)',year)
  if (!version.os_family eq 'Windows') then begin
  search_name2='FMT\Daily\'+yyyy2+mon2+dd2+'*\Wl\*wl*'
  endif
  if (!version.os_family eq 'unix') then begin
  search_name2='FMT/Daily/'+yyyy2+mon2+dd2+'*/Wl/*wl*'
  endif
  files2=findfile(search_name2)
  Nf2=n_elements(files2)
  If2=Nf2-1
  ;print,'File name: ',files2(If2)
  if (julian eq julian0-30.) then begin
   print,'NO DATA OF PHOTOSPHERE!!'
   goto,ended
  endif
 endwhile

;-------------------------------------------------
; Integration of photospheric images during 10 min

ph_img0=intarr(1392,1040)
ph_img1=intarr(1392,1040)

openr,1,files2(If2),/compress
readu,1,ph_img0
close,1

xprof=smooth(total(ph_img0,2),13)
dxprof=xprof-shift(xprof,1)
wx1=where(dxprof(10:1382) eq max(dxprof(10:1382)))+10
wx2=where(dxprof(10:1382) eq min(dxprof(10:1382)))+10
xcen0=(mean(wx1)+mean(wx2))/2.

yprof=smooth(total(ph_img0,1),13)
dyprof=yprof-shift(yprof,1)
wy1=where(dyprof(10:1030) eq max(dyprof(10:1030)))+10
wy2=where(dyprof(10:1030) eq min(dyprof(10:1030)))+10
ycen0=(mean(wy1)+mean(wy2))/2.

;print,'(Xcen0,Ycen0)=',xcen0,ycen0

if (ycen0 ge 470) and (ycen0 le 570) and (xcen0 ge 646) and (xcen0 le 746) then begin
 ph_img=shift(ph_img0*1.,696-xcen0,520-ycen0)
 maisuu=1.
endif else begin
 ph_img=ph_img0*0.
 maisuu=0.
endelse

 dd5=strmid(files2(If2),28,2)
 hh5=strmid(files2(If2),30,2)
 min5=strmid(files2(If2),32,2)
 ss5=strmid(files2(If2),34,2)
 time_sec0=dd5*24.*60.*60. + hh5*60.*60. + min5*60. + ss5*1.
 time_sec1=time_sec0

j=1
while ((time_sec0 - time_sec1) le 600.) and ((If2-j) ge 0) do begin

 dd6=strmid(files2(If2-j),28,2)
 hh6=strmid(files2(If2-j),30,2)
 min6=strmid(files2(If2-j),32,2)
 ss6=strmid(files2(If2-j),34,2)
 time_sec1=dd6*24.*60.*60. + hh6*60.*60. + min6*60. + ss6*1.

 openr,1,files2(If2-j),/compress
 readu,1,ph_img1
 close,1

 xprof=smooth(total(ph_img1,2),13)
 dxprof=xprof-shift(xprof,1)
 wx1=where(dxprof(10:1382) eq max(dxprof(10:1382)))+10
 wx2=where(dxprof(10:1382) eq min(dxprof(10:1382)))+10
 xcen1=(mean(wx1)+mean(wx2))/2.

 yprof=smooth(total(ph_img1,1),13)
 dyprof=yprof-shift(yprof,1)
 wy1=where(dyprof(10:1030) eq max(dyprof(10:1030)))+10
 wy2=where(dyprof(10:1030) eq min(dyprof(10:1030)))+10
 ycen1=(mean(wy1)+mean(wy2))/2.

; print,'(Xcen1,Ycen1)=',xcen1,ycen1

 if (ycen1 ge 470) and (ycen1 le 570) and (xcen1 ge 646) and (xcen1 le 746) then begin
  ph_img=ph_img + shift(1.*ph_img1,696-xcen1,520-ycen1)
  maisuu=maisuu+1.
 endif
 j=j+1

endwhile

if (maisuu eq 0.) then begin
 print,'NO DATA OF PHOTOSPHERE!!'
 goto,ended
endif

ph_img=ph_img/maisuu

fac=1040/512.
a=ph_img(176:1215,*)
a=rotate(a,7)
a=smooth(a,3)
a=1.9*a-smooth(a,11) 
a=bytscl(a) 
b=a(*,253*fac)/5.+a(*,254*fac)/5.+a(*,255*fac)/5.+a(*,256*fac)/5.+a(*,257*fac)/5. 
ww=[0,1,2,3,4,5,506,507,508,509,510,511]*fac 
std=stdev(b(ww),mean2) 
d=a<(max(a)-0)>(mean2+40)  ; Modified at 2008.05.16 &$
d=bytscl(d) 
ph_img(*,*)=0
ph_img(176:1215,*)=d(*,*)

;-------------------------------------------------
; Display of the photospheric image

ph_img1=1.9*ph_img-smooth(ph_img,9) ; Unsharp-mask
tvscl,congrid(ph_img1,1392*0.3,1040*0.3,/interp),0,1040*0.3*0
yyyy4=strmid(files2(If2),22,4)
mon4=strmid(files2(If2),26,2)
dd4=strmid(files2(If2),28,2)
hh4=strmid(files2(If2),30,2)
min4=strmid(files2(If2),32,2)
ss4=strmid(files2(If2),34,2)
timestamp=yyyy4+'-'+mon4+'-'+dd4+' '+hh4+':'+min4+':'+ss4
xyouts,20*0.6,490*0.6+1040*0.3*0,timestamp,/device,chars=1.2,color=255
xyouts,20*0.6+850*0.3,490*0.6+1040*0.3*0,'Solar Photosphrere',/device,chars=1.2,color=255
xyouts,20*0.6+930*0.3,460*0.6+1040*0.3*0,'Hida Obs./FMT',/device,chars=1.2,color=255

ended:
end

