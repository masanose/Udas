;+
; PROCEDURE iug_init
; 
; :DESCRIPTION:
;    Initialize the environment for loading IUGONET data 
;
; :NOTE:
;    This procedure is called automatically on executing most of 
;    iugonet_*.pro.   
; 
; :Examples:
; 
; iug_init
; 
; if !iugonet.data_policy.ear then iug_load_ear, ..... 
; 
; if ~(iugonet.data_policy.sdfit) then begin
;   print, 'Data is not loaded unless you acknowledge the data policy!'
;   return
; endif 
; 
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
; :HISTORY: 
;   2011/12/21: Created
; 
;-
pro iug_init, reset=reset

defsysv,'!iugonet',exists=exists
if (not keyword_set(exists)) or (keyword_set(reset)) then begin

  defsysv,'!iugonet', $
    { $
      init: 0 $
      ,data_policy: { $
                  blr_rish: 0b,       $
                  ltr_rish: 0b,       $
                  wpr_rish: 0b,       $
                  mf_rish: 0b,        $
                  meteor_rish: 0b,    $
                  sdfit:  0b,         $
                  gmag_mm210: 0b,     $
                  gmag_nipr: 0b,      $
                  gmag_wdc:     0b,   $
                  gmag_wdc_dst: 0b,   $
                  gmag_wdc_ae_asy: 0b,$
                  gmag_magdas: 0b,    $
                  ear:    0b,         $    ;iug_load_???? の???の部分を入れる？
                  iprt:    0b,        $
                  eiscat:  0b,        $
                  mu:    0b           $
                } $
    }
    
endif

if keyword_set(reset) then !iugonet.init=0

if !iugonet.init ne 0 then return


!iugonet.init = 1


return
end