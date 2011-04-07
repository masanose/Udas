;+
; PROCEDURE sd_init
; 
; :DESCRIPTION:
;    Initialize the environment for drawing SD data 
;
; :NOTE:
;    This procedure is called automatically on executing most of 
;    sd_*.pro.   
;    
;
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
; :HISTORY: 
;   2010/03/10: Created
; 
; $LastChangedBy: horit $
; $LastChangedDate: 2011-01-10 17:24:09 +0900 (Mon, 10 Jan 2011) $
; $LastChangedRevision: 94 $
; $URL: http://gemsissc.stelab.nagoya-u.ac.jp/svn/ergsc/trunk/erg/ground/radar/superdarn/sd_init.pro $
;-
pro sd_init, reset=reset

defsysv,'!sdarn',exists=exists
if (not keyword_set(exists)) or (keyword_set(reset)) then begin

  defsysv,'!sdarn', $
    { $
      init: 0 $
      ,aacgm_dlm_exists: 0 $
      ,sd_polar: { $
                  plot_time: 0.D, $
                  charsize: 1.0 $
                } $
    }
    
endif

if keyword_set(reset) then !sdarn.init=0

if !sdarn.init ne 0 then return


!sdarn.init = 1

;Check if AACGM DLM is usable?
help, /dlm, 'AACGM', out=out
if strmid(out[0],0,8) eq '** AACGM' then begin
  !sdarn.aacgm_dlm_exists = 1
  aacgm_load_coef, 2000      ;Load the S-H coefficients for Year 2000
endif else begin
  aacgmidl
endelse

;Just compile the AACGM wrapper pro/fun
sd_aacgmlib

return
end