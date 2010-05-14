;+
; NAME:
;       test_leap_yr
; CALLING SEQUENCE:
;       lyr=test_leap_yr(iyr,modays=modays)
; PURPOSE:
;       Determines whether a given year is a leap year, and
;       returns the number of days in every month
; INPUT:
;       iyr = The year, as an integer
; OUTPUT:
;       lyr= 1 if iyr is a leap year and 0 if not
; KEYWORDS:
;       modays= the number of days in each month
;       modays= [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] for
;       a leap yr, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] otherwise
; HISTORY:
;       Written 19-OCt-93 by JM
;       Rewritten, 9-jul-2007, jmm
;-
FUNCTION Test_leap_yr, iyr, modays = modays
   iyr1 = fix(iyr)
   lyr = iyr1 & lyr[*] = 0
   cent = fix(float(iyr1)/100.0)
;   print, iyr1, cent
   ssc1 = where(cent GT 0)
;first be sure that the numbers in iyr1 are less than 100
   IF(ssc1(0) NE -1) THEN iyr1[ssc1] = iyr1[ssc1]-100*cent[ssc1]
;Ok, now do the leap year determination
   test = iyr1 MOD 4
;do the zeros correctly, leap year divisible by 100.0 are not,
;but years divisible by 400.0 are
   test_100 = cent MOD 1
   test_400 = cent MOD 4
   sslyr = where(test EQ 0)
   IF(sslyr(0) NE -1) THEN lyr(sslyr) = 1
   ss_100 = where((iyr1 EQ 0) AND (test_100 EQ 0))
   ss_400 = where((iyr1 EQ 0) AND (test_400 EQ 0))
   IF(ss_100(0) NE -1) THEN lyr(ss_100) = 0
   IF(ss_400(0) NE -1) THEN lyr(ss_400) = 1
;finally
   ssm1 = where(cent LT 0)
   IF(ssm1(0) NE -1) THEN BEGIN
      print, ' NO NEGATIVE CENTURIES'
      lyr(ssm1) = -1
   ENDIF
   modays = intarr(12, 2)
   modays[*, 1] = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   modays[*, 0] = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   RETURN, lyr   
END

;+
;NAME:
; thm_ui_timefix
;PURPOSE:
; Will fix an input time string of yyyy-mm-dd hh:mm:ss.xxxx if there
; is only 1 digit in the day or hour or second, etc.
; Will also return an error message, if the months, days, hours,
; seconds are not valid
;$LastChangedBy: aaflores $
;$LastChangedDate: 2010-01-28 15:17:35 -0800 (Thu, 28 Jan 2010) $
;$LastChangedRevision: 7174 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui/thm_ui_timefix.pro $
Function thm_ui_timefix, time_in, progobj = progobj, _extra = _extra
;-
  
  otp = -1
  t = strtrim(time_in, 2)
;  t = time_string(t)
  pxp1 = strpos(t, '-')         ;You gotta have dashes
  If(pxp1[0] Eq -1) Then Begin
    If(obj_valid(progobj)) Then progobj -> update, 0.0, $
      text = 'Bad time string input, no selection'
    Return, otp
  Endif
  ggg = strsplit(t, '-', /extract)
;Require year-month-day, yy-mm-dd or yyyy-mm-dd
  If(n_elements(ggg) Lt 3) Then Begin
    If(obj_valid(progobj)) Then progobj -> update, 0.0, $
      text = 'Bad time string input, no selection'
    Return, otp
  Endif
;test the year value
  If(is_numeric(ggg[0]) Eq 0) Then Return, otp ;get out immediately     
  yr = fix(ggg[0])
  If(yr ge 90 And yr Lt 100) Then yr = yr+1900 Else Begin
    stx = time_struct(systime(/sec))
    thisyear = stx.year+1       ;add a year
    startyear = 1960
    If(yr Ge 0 And yr Lt 90) Then yr = yr+2000
    If(yr Lt startyear) Then Begin ;removed check for next year, jmm, 3-aug-2009
      If(obj_valid(progobj)) Then progobj -> update, 0.0, $
        text = 'Year out of range, no selection'
      Return, otp
    Endif
  Endelse
;test the month value
  If(is_numeric(ggg[1]) Eq 0) Then Return, otp      
  mo = fix(ggg[1])
  If(mo Lt 1 Or mo Gt 12) Then Begin
    If(obj_valid(progobj)) Then progobj -> update, 0.0, $
      text = 'Month out of range, no selection'
    Return, otp
  Endif
;now, you don't necessarily need hours, minutes, seconds
;first check for a '/', if it's there use it as the time delimiter,
;otherwise use ' '
  pxp2 = strpos(t, '/')
  If(pxp2[0] Ne -1) Then hhh = strsplit(ggg[2], '/', /extract) $
  Else hhh = strsplit(ggg[2], ' ', /extract)
  If(is_numeric(hhh[0]) Eq 0) Then Return, otp      
  dd = long(hhh[0])
;Need leap_yr information
  lyr = test_leap_yr(yr, modays = modays)
  dtest = [1, modays[mo-1, lyr]]
  If(dd Lt dtest[0] Or dd Gt dtest[1]) Then Begin
    If(obj_valid(progobj)) Then progobj -> update, 0.0, $
      text = 'Day out of range, no selection'
    Return, otp
  Endif
  If(dd Lt 10) Then dy = '0'+strcompress(string(dd), /remove_all) $
  Else dy = strcompress(string(dd), /remove_all)
  If(mo Lt 10) Then mon = '0'+strcompress(string(mo), /remove_all) $
  Else mon = strcompress(string(mo), /remove_all)
  yr = strcompress(string(yr), /remove_all)
;the time isn't necessarily passed in
  If(n_elements(hhh) Gt 1) Then Begin
    ttt = strsplit(hhh[1], ':', /extract)
    If(is_numeric(ttt[0]) Eq 0) Then Return, otp      
    hh = long(ttt[0])
    If(hh Lt 0 Or hh Gt 24) Then Begin
      If(obj_valid(progobj)) Then progobj -> update, 0.0, $
        text = 'Hour out of range, no selection'
      Return, otp
    Endif
    If(hh Lt 10) Then hr = '0'+strcompress(string(hh), /remove_all) $
    Else hr = strcompress(string(hh), /remove_all)
    If(n_elements(ttt) Gt 1) Then Begin
      If(is_numeric(ttt[1]) Eq 0) Then Return, otp      
      mm = long(ttt[1])
      If(mm Lt 0 Or mm Gt 59) or (hh eq 24 and mm ne 0) Then Begin  ;also dump if time > '24:00:00'
        If(obj_valid(progobj)) Then progobj -> update, 0.0, $
          text = 'Minute out of range, no selection'
        Return, otp
      Endif
      If(mm Lt 10) Then mn = '0'+strcompress(string(mm), /remove_all) $
      Else mn = strcompress(string(mm), /remove_all)
    Endif Else mn = '00'
    If(n_elements(ttt) Gt 2) Then Begin ;No subsec
      If(is_numeric(ttt[2]) Eq 0) Then Return, otp      
      ss = long(ttt[2]) 
      If(ss Lt 0 Or ss Gt 59) or (hh eq 24 and ss ne 0) Then Begin
        If(obj_valid(progobj)) Then progobj -> update, 0.0, $
          text = 'Second out of range, no selection'
        Return, otp
      Endif
      If(ss Lt 10) Then sc = '0'+strcompress(string(ss), /remove_all) $
      Else sc = strcompress(string(ss), /remove_all)
    Endif Else Begin
      sc = '00'
    Endelse
  Endif Else Begin
    hr = '00' & mn = '00' & sc = '00'
  Endelse
  otp = yr+'-'+mon+'-'+dy+' '+hr+':'+mn+':'+sc

  Return, otp
End

  

  
