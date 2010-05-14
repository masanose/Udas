; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

PRO thm_spin_phase, time_dat,spinpha_int,probe=probe,suffix=suffix, $
                    sunpulse,sunp_spinper
; ----------------------------------------------------------------------
;+
;NAME:
;  thm_spin_phase
;Purpose:
;  Use sunpulse data produced by thm_sunpulse to get spinphase at abitrary times
;Input Parameters:
;      time_dat: double precision array: times of data points at which
;                interpolates are desired.
;Output Parameters:
;   spinpha_int: interpolated spin phase
;Keywords:
;         Probe: a single probe name. e.g. 'a'
;        suffix: suffix on tplot variable (thx_state_sunpulse[_suffix])
;
;Optional Input Parameters (If not present, then state data will be
;   loaded from standard state tplot variables, using probe keyword)
;      sunpulse: double precision array: times of sunpulses
;  sunp_spinper: spin period at each sunpulse time
;
;K. Bromund, SPSystems/NASA/GSFC, May 2007
;$LastChangedBy: bckerr $
;$LastChangedDate: 2007-10-18 18:57:15 -0700 (Thu, 18 Oct 2007) $
;$LastChangedRevision: 1733 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/state/thm_spin_phase.pro $
;-
; ----------------------------------------------------------------------

  if n_params() eq 2 then begin
     if not keyword_set(probe) then begin
        print, 'probe keyword must be set if sunpulse and spinper arrays', $
               ' are not provided.'
        return
     endif else begin
        p = strlowcase(probe)
        if not keyword_set(suffix) then suff = '' else suff = suffix
        name = 'th'+p+'_state_sunpulse'+suff
        thm_sunpulse, probe=p, suffix=suffix
        get_data, name, sunpulse, sunp_spinper, dtype=dt
        if dt ne 1 then begin
           print, '*** thm_spin_phase: no sunpulse data'
           return
        endif
     endelse
  endif

  nsun=N_ELEMENTS(sunpulse)
  ndat=N_ELEMENTS(time_dat)
  spinpha_int=FLTARR(ndat)+!values.f_nan

  PRINT, '   Dimension of sunpulse array = ', nsun
  PRINT, '   Dimension   of   data array = ', ndat
  PRINT, '   Starting interpolation, please wait ...'
  FLUSH, 0, -1, -2

; extrapolation from last sunpulse, given spin period

  j=0L

  FOR i=0L,ndat-1L DO BEGIN

     time_dat_i = time_dat[i]
; look for the closest sunpulse time before the data point
     WHILE j LE nsun-2 && $
        (time_dat_i GT sunpulse[j+1] || time_dat_i lt sunpulse[j] ) DO BEGIN
        j=j+1L
     ENDWHILE

     IF j eq nsun-1 && time_dat_i gt sunpulse[j] + sunp_spinper[j] THEN BEGIN
        PRINT, '*** thm_spin_phase: not enough spin data to determine spin phase'
        print, '    at '+time_string(time_dat_i)+ ' and beyond'
        print, '    last sunpulse: '+time_string(sunpulse[j])
        print, '    spinper      : '+string(sunp_spinper[j])+ ' sec'
        RETURN
     ENDIF

; computation of the interpolated value
; sunp_spinper is assumed to be the exact period between the sunpulse before
; and the sunpulse after the data point.

     tdifA = time_dat_i- sunpulse[j]
     phiA= 360.D*(tdifA)/sunp_spinper[j]

     phiA MOD= (360.D)

     spinpha_int[i]=  phiA

  ENDFOR

END

; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
