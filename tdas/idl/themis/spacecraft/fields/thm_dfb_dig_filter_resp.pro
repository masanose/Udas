

;------------------------------------------------
; Calculates the transfer function for the FIR filters internal to each
; stage in the DFB cascade.  To get the transfer function for a given stage,
; call DFB_transfer (which uses this function as a subroutine).
;   Input:  normalized angular frequency
;   Output: transfer function of FIR filters
;------------------------------------------------
function DFB_FIR_transfer,omega
  ; Calculate transfer function of filter 1
  ; with coefficients [-8,0,72,128,72,0,-8]/256
  I = dcomplex(0,1)
  h = (-8)  + $
        72*(cos(2*omega)-I*sin(2*omega)) + $
       128*(cos(3*omega)-I*sin(3*omega)) + $
        72*(cos(4*omega)-I*sin(4*omega)) + $
      (-8)*(cos(6*omega)-I*sin(6*omega))
  h/=256.

  ; Multiply by transfer function of filter 2
  ; with coefficients [1/4, 1/2, 1/4]
  h *= 0.25 + $
        0.5*(cos(  omega)-I*sin( omega)) + $
       0.25*(cos(2*omega)-I*sin(2*omega))
return,h
end

;------------------------------------------------
; Calculates the transfer function for the DFB filters at a given level
;   Inputs: frequency normalized to sampling Nyquist frequency
;              (i.e. f/4096 or f/8192)
;           filter bank level
;   Output: complex transfer function at given frequencies
;------------------------------------------------
function DFB_transfer,f,level
  ; Use normalized angular frequency
  omega = f*!dpi

  if level eq 0 then return, dcomplexarr(n_elements(f)) + dcomplex(1,0)

  ; For level 1, transfer function is just FIR transfer function
  h = DFB_FIR_transfer(omega)

  ; For higher levels, multiply by all transfer functions,
  ; making sure to appropiately fold in aliasing
  for i=1,level-1 do h*= DFB_FIR_transfer(omega*2^i)

return,h
end

;------------------------------------------------
;+
; function thm_dfb_filter(f, fsamp)
;  purpose: Calculates the transfer function for the DFB digital
;           filters at a given sample rate.
;   Inputs: f     frequency in Hz
;           fsamp Sampling frequency of telemetry signal. 
;                 level of DFB filtering will be calculated 
;                 based on the raw sampling rate of the DC coupled SCM and EFI 
;                 inputs (8192 S/s )
;   Output: transfer function at given frequencies -- abs taken, since
;           phase is adjusted for in L0->L1 processing.
;   Author: Ken Bromund, as a wrapper around routines written by Chris Cully.
;
;$LastChangedBy: kenb-mac $
;$LastChangedDate: 2007-09-24 13:05:40 -0700 (Mon, 24 Sep 2007) $
;$LastChangedRevision: 1620 $ 
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/fields/thm_dfb_dig_filter_resp.pro $
;-
;------------------------------------------------
function thm_dfb_dig_filter_resp, f, fsamp
  level = fix(alog(8192.0/fsamp)/alog(2) + 0.4)
  print, 'Fsamp: ', fsamp, '  Filter bank level: ', level
  if level lt 0 then message, 'fsamp is greater than DFB sampling rate for ' + $
                              'DC-coupled inputs'
  return, abs(DFB_transfer(f/4096.0,level))
end

