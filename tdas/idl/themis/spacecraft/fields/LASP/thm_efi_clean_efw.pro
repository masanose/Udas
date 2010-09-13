;PRO: THM_EFI_CLEAN_EFW
;
;           FOR GENERAL USE: WILL CLEAN UP EFW AND PUT INTO FAC.
;           SPIKE REMOVAL DOES NOT ALWAYS WORK.
;           HINT: USE TRANGE TO ISOLATE SINGLE PARTICLE BURST AND GREATLY REDUCE RUN TIME. 
;  
;SETUP: MAKE SURE thX_state_spinper is available! BEST TO SET UP FOR GSM.
;       MAKE SURE TIMESPAN IS SET!!!!
;PURPOSE:
;    FIXES HF, REMOVES SPIKES, AND LOW FREQUENCY.
;
;INPUT:
;
;KEYWORDS (general):
;    probe          NEEDED: Program does only one sc at a time!
;    Ename          OPTIONAL: Valid TPLOT name for E WAVE, DEFAULT = 'thX_efw' (will fetch)
;    Bdslname       OPTIONAL, Valid TPLOT name for B, DEFAULT = 'thX_fgh_dsl' (will fetch)
;    trange         OPTIONAL. Time range. 
;                   USE TRANGE TO ISOLATE SINGLE PARTICLE BURST AND GREATLY REDUCE RUN TIME!!!! 
;    talk           OPTIONAL. Plots diagnostics. Can be fun. Slow. DEFAULT = 0
;    EfpName        OPTIONAL: Valid TPLOT name for EFP, DEFAULT = 'thX_efp' (will fetch)
;                   EFP is used to find spike positions.
;
;KEYWORDS (Filter):
;    FPole          Pole of high-pass filter. DEFAULT = 5.0 Hz.
;
;KEYWORDS (Remove_Spikes):
;    SpikeRemove    If entered as 0, suppresses spike removal. DEFAULT = 1 (Spikes are removed.)
;    SpikeNwin      Number of points in spike search window. DFLT = 16
;    SpikeSig       Sigma of spikes. DFLT = 5           (Sometimes Sig = 6 works better)
;    SpikeSigmin    Minimun of sigma. DFLT = 0.01 mV/m. (Sometimes Sigmin = 0 works better)
;    SpikeNfit      Number of points in the fit window. DFLT = 16
;    SpikeFit       If set, will do a Gaussian fit to Spikes. DFLT = 0
;
;HISTORY:
;   2009-05-06: REE. First Release.
;-
pro thm_efi_clean_efw, probe=probe, Ename=Ename, Bdslname=Bdslname, $
        trange=trange, talk=talk, EfpName=EfpName, $                 ; GENERAL
        FPole=FPole, $                                               ; FILTER
        SpikeRemove=SpikeRemove, SpikeNwin=SpikeNwin, $              ; REMOVE_SPIKES
        SpikeSig=SpikeSig, SpikeSigmin=SpikeSigmin, $                ; REMOVE_SPIKES
        SpikeNfit=SpikeNfit, SpikeFit=SpikeFit, $                    ; REMOVE_SPIKES
        diagnose=diagnose, wt=wt                                     ; Programmer's use
 
; # CHECK INPUTS - GET NEEDED DATA #
IF not keyword_set(probe) then BEGIN
  print, 'THM_EFI_CLEAN_EFW: SC not set. Exiting...'
  return
ENDIF
sc = probe(0)

; SETDEFAULTS
if n_elements(FPole) EQ 0       then FPole       = 5.0D   ; FILTER
if n_elements(SpikeRemove) EQ 0 then SpikeRemove = 1      ; REMOVE_SPIKES
if n_elements(SpikeNfit) EQ 0   then SpikeNfit   = 400L   ; REMOVE_SPIKES

; CHECK FOR EFW DATA
if not keyword_set(Ename) then Ename = 'th' + sc + '_efw'
IF thm_check_tvar(Ename) then BEGIN  
  get_data, ename(0), data=E, dlim=elim  
ENDIF ELSE BEGIN
  thm_load_efi, probe=sc, datatype=['efw', 'vaw'], coord='dsl', trange=trange
  Ename = 'th' + sc + '_efw'
  get_data, ename(0), data=E, dlim=elim
  IF data_type(E) NE 8 then BEGIN
    print, 'THM_EFI_CLEAN_EFW: Cannot get electric field data. Exiting...'
   return
  ENDIF
ENDELSE

; CHECK FOR MAG DATA
if not keyword_set(Bdslname) then Bdslname = 'th' + sc + '_fgh_dsl'
IF thm_check_tvar(Bdslname) then BEGIN                         
  get_data, Bdslname(0), data=Bdsl, dlim=blim                  
ENDIF ELSE BEGIN                                               
  print, 'THM_EFI_CLEAN_EFP: Mag data not stored in dsl. Fetching...'
  thm_load_fgm, probe=sc, datatype = ['fgh'], coord=['dsl'], trange=trange, suffix = '_dsl'
  Bdslname = 'th' + sc + '_fgh_dsl'
  get_data, Bdslname(0), data=Bdsl, dlim=blim
  IF data_type(Bdsl) NE 8 then BEGIN
    print, 'THM_EFI_CLEAN_EFP: Cannot get MAG data. Exiting...'
    return
  ENDIF
ENDELSE

; CHECK FOR SPIN DATA
SpinName = 'th' + sc + '_state_spinper'                    
IF thm_check_tvar(Spinname) then BEGIN  
 get_data, SpinName, data=SpinPer                          
ENDIF ELSE BEGIN                                               
  thm_load_state, probe=sc, datatype='spinper'           
  get_data, SpinName, data=SpinPer   
  IF data_type(SpinPer) NE 8 THEN BEGIN
    print, 'THM_EFI_CLEAN_EFP: Cannot get spin period. Exiting...'
    return
  ENDIF
ENDELSE                                                  

; GET EFP DATA FOR SPIKE FINDER
IF keyword_set(SpikeRemove) then BEGIN
  if not keyword_set(EfpName) then EfpName = 'th' + sc + '_efp'
  IF thm_check_tvar(EfpName) then BEGIN                       
    get_data, EfpName(0), data=Efp 
  ENDIF ELSE BEGIN
    thm_load_efi, probe=sc, datatype=['efp', 'vap'], coord='dsl', trange=trange
    get_data, EfpName(0), data=Efp
    IF data_type(Efp) NE 8 then BEGIN
      print, 'THM_EFI_CLEAN_EFW: Cannot get EFP data. Spikes cannot be removed.'
      SpikeRemove = 0
    ENDIF
  ENDELSE
ENDIF

; CLIP DATA TO RANGE
IF keyword_set(trange) then BEGIN
   trange_clip, E, trange(0), trange(1), /data, BadClip=BadEclip
   trange_clip, Bdsl, trange(0), trange(1), /data, BadClip=BadBclip
   trange_clip, SpinPer, trange(0)-60.d, trange(1)+60.d, /data, BadClip=BadSclip
   IF (keyword_set(BadEclip) OR keyword_set(BadBclip) OR keyword_set(BadSclip) ) THEN BEGIN
     print, 'THM_EFI_CLEAN_EFW: Problem with trange clip. Exiting...'
     print, '0=OK; 1=Problem. E:', BadEclip, 'B:', BadBclip, 'Spin:', BadSclip
     return
   ENDIF
ENDIF

; MAKE ARRAY FOR FAC STORAGE
Efac = E

; ## IDENTIFY INDIVIDUAL WAVE BURSTS  ##
tE   = E.x
thm_lsp_find_burst, E, istart=bstart, iend=bend, nbursts=nbursts, mdt=mdt

; START LOOP OVER INDIVIDUAL BURSTS
FOR ib=0L, nbursts-1 DO BEGIN

  print, 'BURST: ', ib+1, ' out of: ', nbursts

  ; BREAK OUT DATA
  t  = tE(bstart(ib):bend(ib))
  Ex = E.y(bstart(ib):bend(ib),0)
  Ey = E.y(bstart(ib):bend(ib),1)
  Ez = E.y(bstart(ib):bend(ib),2)
  Ttemp = [min(t)-mdt/2, max(t)+mdt/2]  

  ; CALCULATE SPIn PERIOD
  ind = where( (SpinPer.x GE (Ttemp(0)-60.d)) AND $
    (SpinPer.x LE (Ttemp(1)+60.d)), nind)
  IF nind EQ 0 then BEGIN
    print, 'THM_EFI_CLEAN_EFW: Spin period missing during burst. Exiting...'
    return
  ENDIF
  per = median(spinper.y(ind))

  ; DO HIGH-PASS ON E
  if keyword_set(talk) then print, 'HIGH-PASS FILTER'
  Exf = thm_lsp_filter_fft(Ex, mdt, FPole, 1.d5)
  Eyf = thm_lsp_filter_fft(Ey, mdt, FPole, 1.d5)
  Ezf = thm_lsp_filter_fft(Ez, mdt, FPole, 1.d5)

  ; REMOVE SPIKES
  IF keyword_set(SpikeRemove) then BEGIN
    thm_lsp_remove_spikes, t, Exf, Eyf, Ezf, per, Efp=Efp, Nwin=SpikeNwin, $
              SpikeSig=SpikeSig, Sigmin=SpikeSigmin, Nfit=SpikeNfit, Fit=SpikeFit, $
              talk=talk, diagnose=diagnose, wt=wt
  ENDIF

  ; FIX FREQUENCY AND PAHSE
  Exf = thm_efi_fix_freq_and_phase(Exf)
  Eyf = thm_efi_fix_freq_and_phase(Eyf)
  Ezf = thm_efi_fix_freq_and_phase(Ezf,/ax)

  ; SAVE E DATA
  E.y(bstart(ib):bend(ib),0)    = Exf
  E.y(bstart(ib):bend(ib),1)    = Eyf
  E.y(bstart(ib):bend(ib),2)    = Ezf

  ; DESPIN THE E DATA
  Eclip = E
  trange_clip, Eclip, Ttemp(0), Ttemp(1), /data
  gain = [1.D, 1.D, 1.D]
  offset = [0.0D, 0.0D, 0.0D]
  store_data, 'Eclip', data=Eclip, dlim=elim
  thm_efi_despin, sc, 'efw', offset, gain, $
                tplot_name='Eclip', newname= 'Eclip'

  ; CLIP MAGNETOMETER DATA
  Bclip = Bdsl
  trange_clip, Bclip, Ttemp(0)-3.d, Ttemp(1)+3.d, /data
  store_data, 'Bclip', data=Bclip, dlim=blim
  tsmooth2, 'Bclip', 11, newname='Bclip'

  ; GO TO FAC COORDINATES (JIANBAO)
  thm_fac_matrix_make, 'Bclip', other_dim='zdsl', $
             newname='th'+sc+'_fgh_fac_mat'
  tvector_rotate, 'th'+sc+'_fgh_fac_mat', 'Eclip', $
          newname='Eclip', error=error

  ; GET ECLIP AND SAVE
  get_data, 'Eclip', data=Eclip
  Efac.y(bstart(ib):bend(ib),0)    = Eclip.y(*,0)
  Efac.y(bstart(ib):bend(ib),1)    = Eclip.y(*,1)
  Efac.y(bstart(ib):bend(ib),2)    = Eclip.y(*,2)
  
ENDFOR
; ## END OF LOOP

; STORE E DATA
; add BAND to data_att -JBT
data_att = {DATA_TYPE: elim.data_att.DATA_TYPE, $
            COORD_SYS: elim.data_att.COORD_SYS, $
            UNITS: elim.data_att.UNITS, $
            CAL_PAR_TIME: elim.data_att.CAL_PAR_TIME, $
            OFFSET: elim.data_att.OFFSET, $
            EDC_GAIN: elim.data_att.EDC_GAIN, $
            EAC_GAIN: elim.data_att.EAC_GAIN, $
            BOOM_LENGTH: elim.data_att.BOOM_LENGTH, $
            BOOM_SHORTING_FACTOR: elim.data_att.BOOM_SHORTING_FACTOR, $
            DSC_OFFSET: elim.data_att.DSC_OFFSET, $
            BAND: '~' + string(FPole, format='(I0)') + ' Hz - ~3.3 kHz'}
                     ; BAND - the freq band of the data

ename2 = ename + '_clean_dsl'
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: elim.colors, $
  LABELS: ['Ex', 'Ey', 'Ez'], LABFLAG: elim.labflag, $
  YTITLE: 'E_DSL (th' + sc +')'}
store_data, ename2[0], data=E, dlim=dlim

; STORE FAC DATA
efacname = ename + '_clean_fac'
perp1 = 'E!DSP!N'
perp2 = 'E!Dperp!N'
para = 'E!D||!N'
data_att.coord_sys = 'fac: x in spin-plane'
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: elim.colors, $
  LABELS: [perp1, perp2, para], LABFLAG: elim.labflag, $
  YTITLE: 'E_FAC (th' + sc +')'}
store_data, efacname[0], data=Efac, dlim=dlim

store_data, ['Eclip', 'Bclip'], /delete
end











