;+
; PRO: THM_EFI_FIX_HF, dname, freq=freq, ghf=ghf, dt=dt
;
; PURPOSE: A routine which calls THM_EFI_FIX_FREQ_AND_PHASE to fix the gain and remove the
;          phase shift due to resistive to capacitive crossover of the  
;          THEMIS preamps. ONLY USEFUL FOR BURST DATA.
;
; INPUT: 
;       danme -       REQUIRED. tplot data to be fixed. 
;
; KEYWORDS: 
;       freq -        OPTIONAL. Crossover frequency in HZ. DEFAULT = SPIN PLANE
;                     Careful! Derived from Rsheath(Csheath+Cin)
;       ghf -         OPTIONAL. Gain at high frequency. DEFAULT = SPIN PLANE
;                     Csheath/(Csheath+Cin)
;       dt -          OPTIONAL. Time between samples (s). DEFAULT = 1/8192
;
; CALLING: Esp = thm_efi_fix_freq_and_phase(Esp) or 
;          Eax = thm_efi_fix_freq_and_phase(Ex, /ax)
;
; OUTPUT: Data is integrated to remove gain/phase error from preamp RC 
;         crossover.
;
;
; INITIAL VERSION: REE 08-08-26
; MODIFICATION HISTORY: 
; LASP, CU
; 
;-
pro thm_efi_fix_hf, dname, freq=freq, ghf=ghf, dt=dt

; GET DATA
get_data, dname, data=data
IF not keyword_set(data) then BEGIN
  print, 'DATA NAME NOT VALID'
  return
ENDIF

; ISOLATE INDIVIDUAL COMPONENTS
Ex = data.y[*,0]
Ey = data.y[*,1]
Ez = data.y[*,2]

; FIX THEM
Ex = thm_efi_fix_freq_and_phase(Ex)
Ey = thm_efi_fix_freq_and_phase(Ey)
Ez = thm_efi_fix_freq_and_phase(Ez,/ax)

; RECONSTRUCT
data.y[*,0] = Ex 
data.y[*,1] = Ey 
data.y[*,2] = Ez

store_data, dname, data=data

return
end