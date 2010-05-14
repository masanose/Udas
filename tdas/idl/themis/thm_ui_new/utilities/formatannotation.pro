
;helper function
; Removes trailing zeros and/or decimal from string,
; Assumes trailing spaces have already been removed.
function remove_zeros, sval

    compile_opt idl2, hidden
  
  f = stregex(sval, '\.?0*$',length=len)
  
  return, strmid(sval, 0, (strlen(sval)-len) )

end

;+
;FUNCTION:
;  formatannotation
;
;PURPOSE:
;  This routine is used as a callback for axis labeling by IDLgrAxis
;  Because it is a callback routine, IDL requires it to have this specific form.
;  It is probably useful as a general purpose formatting routine, as well.
;   
;Inputs:
;Axis:Required by IDL, but ignored
;Index: Required by IDL, but ignored
;Value: Required The value to be formatted.
;Data:  Keyword,Required
;  The data struct holds the important information and has the following format:
;  data = {timeAxis:0B,  $ ;Should we format as time data?
;        formatid:0L,  $ ;Precision of returned value in sig figs 
;        scaling:0B,   $ ;0:Linear,1:Log10,2:LogN
;        exponent:0B,  $ ;(REQUIRED for non-time data)0:Auto-Format(see description),1:Numerical(double),2:Sci-Notation(1.2x10^3)
;        range:[0D,0D]}$ ; The range field is optional.  If it is present in the struct, value will be interpreted a multiplier over the specified range.(generally this is used if the data is being stored as a proportion)
;
;Auto-Format:
;If the number being formatted is too large or small to be displayed with the requested number of
;  significant figures then the number will be automatically displayed in scientific notation
;  (e.g. for 3 sig. figs '1234.5' will be shown as '1.23x10^3' and '.00012345' will be shown as '1.23x10^-4') 
;Values in log_10() space will be displayed as '10^1.2'
;Values in ln() space will be displayes as 'e^1.2'
;Integer formatting is not effected.
;
;Formatting Codes:
;Formatting codes are utilized to create superscripts and unicode characters that are displayd
;by IDLgraxis:
;  !z(00d7): Unicode multiplication symbol
;  !U      : Superscripts the following substring  
;
;Example:
;  print,formatannotation(0,0,1,data={timeaxis:0,formatid:7,scaling:0,exponent:0})
;  1.000000
;  print,formatannotation(0,0,.5,data={timeaxis:1,formatid:12,scaling:0,range:[time_double('2007-03-23'),time_double('2007-03-24')]})
;  082/12:00:00.000
;  print,formatannotation(0,0,4,data={timeaxis:0,formatid:5,scaling:2,exponent:0})
;  e!U4.0000
;  print,formatannotation(0,0,.25,data={timeaxis:0,formatid:5,scaling:1,range:[1,2],exponent:0})
;  10!U1.2500
;  print,formatannotation(0,0,1234,data={timeaxis:0,formatid:7,scaling:0,exponent:2})
;  1.234000!z(00d7)10!U3
;  
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-08-06 14:15:28 -0700 (Thu, 06 Aug 2009) $
;$LastChangedRevision: 6537 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/formatannotation.pro $
;-
function formatannotation,axis,index,value,data=data
 
  compile_opt idl2
  
  ;CONSTANTS
  months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
  expformat = '!U'
  numLimit = 10000000000
  roundingfactor = 1d-15
  
  ;print,value
  ;help,data,/str
  
 ; data.exponent = 1

  if finite(value,/nan) then begin
    ;print,strtrim(string(value))
    return,strtrim(string(value))
  endif 

  if in_set('range',strlowcase(tag_names(data))) then begin

    
     val = (value + data.range[0]/(data.range[1]-data.range[0]))*(data.range[1]-data.range[0])
     ;round values near 0
     relativecuttoff = (data.range[1]-data.range[0])*roundingfactor
     if val le relativecuttoff && val ge -relativecuttoff then begin
       val = 0
     endif
;    if ~data.timeaxis then begin
;      rval = round(val)
;      if abs(val - rval) lt 10d^(-11) then val = rval
;    endif
  endif else begin
    val = value
  endelse

;Calculate real space value if necessary
  oval = 1d*val
  if data.scaling eq 1 then begin
    val = 10d^val
  endif else if data.scaling eq 2 then begin
    val = 1d*exp(val)
  endif else begin
    val = 1d*val
  endelse
  
  if ~finite(val) then begin
  ;  print,strtrim(string(val))
    return,strtrim(string(val))
  endif 

;  if in_set('formatstyle',strlowcase(tag_names(data))) then begin
;    formatstyle = data.formatstyle
;  endif else begin
;    formatstyle = 0
;  endelse


  ;correction factor for variation in exponential
  ;format across OS's
  ;format on Linux/Unix/OSX = '2.E+00'
  ;format on Windows = '2.E+000'
;  if !VERSION.OS_FAMILY eq 'Windows' then begin
;    os = 1
;  endif else begin
;    os = 0
;  endelse

  ;time annotations
  if data.timeAxis then begin
    
    ;round to nearest millisecond
    tm = (val - floor(val))*1000d
    val = floor(val) + round(tm)*10d^(-3)
    
    ts = time_struct(val)
    
    if data.formatid eq 0 then begin
      return,string(ts.year,format='(I4.4)') + '-' + $
             months[ts.month-1] +'-'+ $
             string(ts.date,format='(I2.2)')
    endif else if data.formatid eq 1 then begin
      return,string(ts.year,format='(I4.4)') + '-' + $
             months[ts.month-1] +'-'+ $
             string(ts.date,format='(I2.2)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)')
    endif else if data.formatid eq 2 then begin
      return,string(ts.year,format='(I4.4)') + '-' + $
             months[ts.month-1] +'-'+ $
             string(ts.date,format='(I2.2)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)')
    endif else if data.formatid eq 3 then begin
      return,string(ts.year,format='(I4.4)') + '-' + $
             months[ts.month-1] +'-'+ $
             string(ts.date,format='(I2.2)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)') + '.' + $
             string(ts.fsec*1000,format='(I3.3)')
    endif else if data.formatid eq 4 then begin
      return,string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)')
    endif else if data.formatid eq 5 then begin
      return,string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)')    
    endif else if data.formatid eq 6 then begin
      return,string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)') + '.' + $ 
             string(ts.fsec*1000,format='(I3.3)')
    endif else if data.formatid eq 7 then begin
      return,string(ts.month,format='(I2.2)') + ':' + $
             string(ts.date,format='(I2.2)')
    endif else if data.formatid eq 8 then begin
      return,string(ts.month,format='(I2.2)') + '-' + $
             string(ts.date,format='(I2.2)')+ '/' + $        
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)')
    endif else if data.formatid eq 9 then begin
      return,string(ts.doy,format='(I3.3)')
    endif else if data.formatid eq 10 then begin
      return,string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)')
    endif else if data.formatid eq 11 then begin
      return,string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)')
    endif else if data.formatid eq 12 then begin
      return,string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)') + '.' + $
             string(ts.fsec*1000,format='(I3.3)')
    endif else if data.formatid eq 13 then begin      
      return,string(ts.year,format='(I4.4)') + '-' + $
             string(ts.doy,format='(I3.3)')
    endif else if data.formatid eq 14 then begin  
      return,string(ts.year,format='(I4.4)') + '-' + $
             string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)')
    endif else if data.formatid eq 15 then begin  
      return,string(ts.year,format='(I4.4)') + '-' + $
             string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)')
    endif else if data.formatid eq 16 then begin
      return,string(ts.year,format='(I4.4)') + '-' + $
             string(ts.doy,format='(I3.3)') + '/' + $
             string(ts.hour,format='(I2.2)') + ':' + $
             string(ts.min,format='(I2.2)') + ':' + $
             string(ts.sec,format='(I2.2)') + '.' + $ 
             string(ts.fsec*1000,format='(I3.3)')
     endif else begin
       ok = error_message('Illegal annotation format',/traceback)
       return,''
     endelse

;Data Annotations Formating
  endif else begin
 
;Initializations for formatting    
    prefix = ''
    suffix = ''
    neg=0
    dec=0
    use_oval = 0
    precision = data.formatid-1 > 0  ;desired decimal precision (1 less than sig figs)
    negzero = ( (val eq 0) && (strmid(strtrim(val,1),0,1) eq '-') ) ? 1:0


;Determine format type:
;--------
;The type of annotation to be returned is chosen below by setting the 'type' variable.
;0 - numerical format
;1 - sci-notation (also requires expsign=1 or -1)
;2 - e^x format
;3 - 10^x format

 thm_ui_usingexponent,val,data,type=type,expsign=expsign

 ;if an error occurred return
 if type eq -1 then return,''

;Handle double format
;--------------
  if type eq 0 then begin
    
    thm_ui_getlengthvars, val, dec, neg
  
    if data.formatid eq 0 then begin
      formatString = '(I' + strtrim(string(neg+dec,format='(I)'),2)+')'
      if precision lt dec then precision = dec < 9
    endif else begin
    
      ;increment length if rounding will add digit
      check_dround, val, neg, dec, precision
      
      p0 = abs(val) lt 1 ? 1:0 
      prec = ((precision+1-dec+p0) > 0)

     ; if data.formatid le 10 then begin
      if data.formatid le 16 then begin
        formatString = '(D' + strtrim(string(neg+dec+prec+1+negzero,format='(I)'),2)+$
                         '.' + strtrim(prec,2)+')'
      endif else begin
         ;ok = error_message('Illegal annotation format',traceback=1)
         ;print, 'Something is initialized wrong.'
         ;print, 'Format: ' + strtrim(data.formatid,2) + '  Value: ' + string(val) 
         return,''
      endelse
    endelse
    
;Handle exponential (sci-notation) format
;--------------
  endif else if type eq 1 then begin
    
    thm_ui_getlengthvars, val, dec, neg
    
    if val eq 0 then $
      return, remove_zeros(string(val,format='(D'+strtrim(precision+2+negzero)+'.'+strtrim(precision)+')'))
    
    ;determine exponent (1,-1, 0)
    if expsign eq -1 then begin
      esign = '-'
      exponent = ceil(abs(alog10(abs(val))))
    endif else if expsign eq 1 then begin
      esign = ' '
      exponent = floor(abs(alog10(abs(val))))
    endif else if expsign eq 0 then begin
      esign = ' '
      exponent = 0
    endif
    
    val = val * (10d^exponent)^(-expsign)

    ;increment length if rounding will add digit
    check_eround, val, neg, dec, precision, exponent
    
    ;add desired exponent string
    if abs(val) ge 0 and abs(val) lt 10 then begin
      suffix = ' !3x!X 10'+ expformat + esign + strtrim(exponent,2)
    endif else begin
      ;print, 'FormatAnnotation: Error creating exponent!'
      return,''
    endelse
    
    if data.formatid eq 0 then begin
      formatString = '(I' + strtrim(string(neg+dec,format='(I)'),2)+')'
    endif else begin
      formatString = '(D' + strtrim(string(neg+precision+2,format='(I)'),2)+'.' + $
                       strtrim(precision,2) + ')'
    endelse
  
;Handle e^x format, number should still be in ln() space at this point
;--------------
  endif else if type eq 2 then begin
  
    if finite(oval, /infinity) then begin
      if oval lt 0 then $
        return, remove_zeros(string(0d,format='(D'+strtrim(precision+2)+'.'+strtrim(precision)+')')) $
          else return, 'Infinity'
    endif 
  
    thm_ui_getlengthvars, oval, dec, neg
    use_oval = 1
    
    ;increment length if rounding will add digit to exponent
    check_dround, oval, neg, dec, precision

    prec = ((precision+1-dec) > 0)
    formatString = '(D' + strtrim(string(neg+dec+prec+1+negzero,format='(I)'),2)+'.' + $
                     strtrim(prec,2) + ')'

    prefix = 'e' + expformat
    

;Handle 10^x format, number should still be in log_10() space at this point
;--------------
  endif else if type eq 3 then begin
  
    if finite(oval, /infinity) then begin
      if oval lt 0 then $
        return, remove_zeros(string(0d,format='(D'+strtrim(precision+2)+'.'+strtrim(precision)+')')) $
          else return, 'Infinity'
    endif 
    
    thm_ui_getlengthvars, oval, dec, neg
    use_oval = 1

    ;increment length if rounding will add digit to exponent
    check_dround, oval, neg, dec, precision

    prec = ((precision+1-dec) > 0)
    formatString = '(D' + strtrim(string(neg+dec+prec+1+negzero,format='(I)'),2)+'.' + $
                     strtrim(prec,2) + ')'

    prefix = '10' + expformat

  endif else begin
    ok = error_message('Illegal annotation type',/traceback)
    return,''
  endelse 


;check that the format did not exceed max length
  if stregex(formatstring, '[0,1,2,3,4,5,6,7,8,9]+',/extract) ge 255 then return, 'Overflow'

;  if stregex( string(val,format=formatString), '[*]+', /bool) then begin
;    print, val, formatstring
;    stop
;  endif

  if use_oval then begin
    return, prefix + remove_zeros(string(oval,format=formatString)) + suffix 
  endif else begin
    if data.formatid eq 0 then return, prefix + string(val,format=formatString) + suffix
    return, prefix + remove_zeros(string(val,format=formatString)) + suffix
  endelse
  
  endelse

end

