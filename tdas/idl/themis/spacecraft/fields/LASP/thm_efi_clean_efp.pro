;+
;PRO: THM_EFI_CLEAN_EFP
;
;           FOR GENERAL USE: WILL CLEAN UP EFP AND PUT INTO FAC AND GSM COORDINATES.
;           DEFAULTS SET FOR TAIL. USE /SUBSOLAR FOR DAYSIDE DEFAULTS.
;           MERGE CAN EXHIBIT PATHOLOGICAL BEHAVIOR IF B IS NEAR THE SPIN PLANE.
;           SPIKE REMOVAL DOES NOT ALWAYS WORK.
;           HINT: USE TRANGE TO ISOLATE SINGLE PARTICLE BURST AND GREATLY REDUCE RUN TIME. 
;  
;SETUP: MAKE SURE thX_state_spinper is available! BEST TO SET UP FOR GSM.
;       MAKE SURE TIMESPAN IS SET!!!!
;
;PURPOSE:
;    FIXES AXIAL, REMOVES SPIKES, SPINTONE, SC POTENTIAL, AND OTHER EFP ERRORS.
;
;INPUT:
;
;KEYWORDS (general):
;    probe          NEEDED: Program does only one sc at a time!
;    Ename          OPTIONAL: Valid TPLOT name for E, DEFAULT = 'thX_efp' (will fetch)
;    Vname          OPTIONAL: Valid TPLOT name for E, DEFAULT = 'thX_vap' (will fetch)
;    Bdslname       OPTIONAL, Valid TPLOT name for B, DEFAULT = 'thX_fgh_dsl' (will fetch)
;    trange         OPTIONAL. Time range. 
;                   USE TRANGE TO ISOLATE SINGLE PARTICLE BURST AND GREATLY REDUCE RUN TIME!!!! 
;    subsolar       OPTIONAL. If set, defaults for subsolar region are used. DEFAULT = TAIL
;    talk           OPTIONAL. Plots diagnostics. Can be fun. Slow. DEFAULT = 0
;
;KEYWORDS (Remove_SpinTone):
;    SpinRemove     If entered as 0, suppresses spintone removal. DEFAULT = 4 (Spintone removed)
;                   1: Only spintone, 2: Spintone and 2nd harm. 3: Spintone and 4th harm.
;                   4: Spintone, 2nd, and 4th harm.
;    SpinNsmooth    Activates median smoothing. # of half periods. Must be odd. DEFAULT = 19
;    SpinPoly       Activates polyfit smoothing. Can be unstable if >9. DEFAULT = 0
;
;KEYWORDS (Remove_Potential):
;    VscRemove      If entered as 0, suppresses potential removal. DEFAULT = 1 (Potential removed.)
;    VscPole        Frequency to median smooth Vsc before applying to Ez. DEFAULT = 5.0 Hz
;    Vpoly          Actives polyfit of Ez to Vsc.  DEFAULT = 2 (5 /Subsolar)
;    Use_Electrons  If set, Te is used to modify Vsc before removal. DEFAULT = 0 (1 /SubSolar)
;    TeName         OPTIONAL: Valid TPLOT name for Te. DEFAULT = 'thX__peeb_avgtemp' (will fetch)
;
;KEYWORDS (Remove_Spikes):
;    SpikeRemove    If entered as 0, suppresses spike removal. DEFAULT = 1 (Spikes are removed.)
;    SpikeNwin      Number of points in spike search window. DFLT = 16
;    SpikeSig       Sigma of spikes. DFLT = 5           (Sometimes Sig = 6 works better)
;    SpikeSigmin    Minimun of sigma. DFLT = 0.01 mV/m. (Sometimes Sigmin = 0 works better)
;    SpikeNfit      Number of points in the fit window. DFLT = 16
;    SpikeFit       If set, will do a Gaussian fit to Spikes. DFLT = 0
;
;KEYWORDS (Remove_Spin_Epoch):
;    EpochRemove    If set, removes all spin-epoch signals. DEFAULT = 0 (Not used.)
;                   Only useful for short stretches or if plasma condtitions are constant.
;
;KEYWORDS (Fix_Axial)
;    FixAxial       If entered as 0, suppresses axial fix. DEFAULT=1 (Axial is fixed.)
;    AxPoly         Forces polynomial fit of Ez-Eder of order AxPoly. DEFAULT=2 (9 /SubSolar)
;    Merge          Merges Ederived with Eaxial.  DEFAULT = 0 No merging (1 /Subsolar)
;                   1 - Abrupt merging. If Eder is avaliable Eder is used (0 - Fmerge). Else Ez.
;                   2 - Soft merging. Bz/|B| is considered when merging.
;                   WARNING: MERGE=1 OR 2 CAN EXHIBIT PATHOLOGICAL BEHAVIOR IF MAGNETIC FIELD 
;                            IS NEAR THE SPIN PLANE. PLEASE DOUBLE CHECK RESULTS IF USED. 
;    Fmerge         Frequency of crossover for merging Ederived with Eaxial. 
;                   DEFAULT = 5.0 Hz (/Subsolar). 
;    MergeRatio     Mimimum value of Bz/|B| to start merge (USED IF MERGE=2). DEFAULT = 0.05
;    MinBz          Needed to avoid divide by zero in Ederive. DEFAULT = 0.01 nT
;    MinRatio       Mimimum value of Bz/|B| to calculate Ederive. DEFAULT = 0.1 (0.05 /Subsolar)
;
;KEYWORDS (B_Smooth)
;    BspinPoly      Activates polyfit smoothing. Can be unstable if >9. DEFAULT = 2. -1 suppresses.
;    Bsmooth        Activates B smoothing for fac rotation. DEFAULT = 11. 1 or less suppresses.
;            
;
;
;HISTORY:
;   2009-05-05: REE. First Release.
;   2009-06-04: REE/JBT. Second Release.
;-

pro thm_efi_clean_efp, probe=probe, Ename=Ename, Vname=Vname, Bdslname=Bdslname, $
        trange=trange, talk=talk, $                                  ; GENERAL
        subsolar=subsolar, $                                         ; DEFAULTS
        SpinNsmooth=SpinNsmooth, SpinPoly=SpinPoly, $                ; REMOVE_SPINTONE
        SpinRemove=SpinRemove, $                                     ; REMOVE_SPINTONE
        VscPole=VscPole, Vpoly=Vpoly, VscRemove=VscRemove, $         ; REMOVE_POTENTIAL
        use_electrons=use_electrons, TeName=TeName, $                ; REMOVE_POTENTIAL
        SpikeRemove=SpikeRemove, SpikeNwin=SpikeNwin, $              ; REMOVE_SPIKES
        SpikeSig=SpikeSig, SpikeSigmin=SpikeSigmin, $                ; REMOVE_SPIKES
        SpikeNfit=SpikeNfit, SpikeFit=SpikeFit, $                    ; REMOVE_SPIKES
        EpochRemove=EpochRemove, $                                   ; REMOVE_SPIN_EPOCH
        FixAxial=FixAxial, AxPoly=AxPoly, $                          ; FIX_AXIAL
        Merge=Merge, FMerge=FMerge, MergeRatio=MergeRatio, $         ; FIX_AXIAL
        MinBz=MinBz, MinRatio=MinRatio, $                            ; FIX_AXIAL (E_DERIVE)
        BspinPoly=BspinPoly, Bsmooth=Bsmooth                         ; B_SMOOTH

; # CHECK INPUTS - GET NEEDED DATA #
IF not keyword_set(probe) then BEGIN
  print, 'THM_EFI_CLEAN_EFP: SC not set. Exiting...'
  return
ENDIF
sc = probe(0)

; CHECK FOR SUBSOLAR KEYWORD
IF keyword_set(subsolar) then BEGIN
  if n_elements(Vpoly) EQ 0         then Vpoly         = 4      ; REMOVE_POTENTIAL
  if n_elements(use_electrons) EQ 0 then use_electrons = 1      ; REMOVE_POTENTIAL
  if n_elements(Axpoly) EQ 0        then Axpoly        = 7      ; FIX_AXIAL
  if n_elements(Merge) EQ 0         then Merge         = 1      ; FIX_AXIAL
  if n_elements(MinRatio) EQ 0      then MinRatio      = 0.05d  ; FIX_AXIAL
  if n_elements(SpinNsmooth) EQ 0   then SpinNsmooth   = 19     ; REMOVE_SPINTONE
ENDIF

; SET TAIL DEFAULTS
if n_elements(SpinRemove) EQ 0  then SpinRemove  = 4      ; REMOVE_SPINTONE
if n_elements(SpinNsmooth) EQ 0 then SpinNsmooth = 39     ; REMOVE_SPINTONE
if n_elements(VscRemove) EQ 0   then VscRemove   = 1      ; REMOVE_POTENTIAL
if n_elements(VscPole) EQ 0     then VscPole     = 5.0d   ; REMOVE_POTENTIAL
if n_elements(Vpoly) EQ 0       then Vpoly       = 2      ; REMOVE_POTENTIAL
if n_elements(SpikeRemove) EQ 0 then SpikeRemove = 1      ; REMOVE_SPIKES
if n_elements(FixAxial) EQ 0    then FixAxial    = 1      ; FIX_AXIAL
if n_elements(Axpoly) EQ 0      then Axpoly      = 2      ; FIX_AXIAL
if keyword_set(merge)           then Fmerge      = 5.0d   ; FIX_AXIAL
if not keyword_set(merge)       then Fmerge      = 0      ; FIX_AXIAL
IF keyword_set(merge) then BEGIN
  if merge EQ 2 then softmerge=1
ENDIF
if n_elements(BspinPoly) EQ 0   then BspinPoly   = 2      ; B_SMOOTH
if n_elements(Bsmooth) EQ 0     then Bsmooth     = 11     ; B_SMOOTH

; CHECK FOR EFP DATA
if not keyword_set(Ename) then Ename = 'th' + sc + '_efp'
IF thm_check_tvar(Ename) then BEGIN                       ; added by JBT
  get_data, ename(0), data=E, dlim=elim                   ; added by JBT
ENDIF ELSE BEGIN
  thm_load_efi, probe=sc, datatype=['efp', 'vap'], coord='dsl', trange=trange
  get_data, ename(0), data=E, dlim=elim
  IF data_type(E) NE 8 then BEGIN
    print, 'THM_EFI_CLEAN_EFP: Cannot get electric field data. Exiting...'
   return
  ENDIF
ENDELSE

; CHECK FOR VOLTAGES
if not keyword_set(Vname) then Vname = 'th' + sc + '_vap'
if not keyword_set(Vscname) then Vscname = 'th' + sc + '_vsc'
IF thm_check_tvar(Vscname) then BEGIN                          
  get_data, Vscname[0], data=Vsc                                  
ENDIF ELSE BEGIN                                               
  thm_efi_get_potential, Vname(0), trange=trange
  get_data, Vscname[0], data=Vsc
    IF data_type(Vsc) NE 8 then BEGIN
      print, 'THM_EFI_CLEAN_EFP: Cannot get SC potential. Exiting...'
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
 get_data, SpinName[0], data=SpinPer                          
ENDIF ELSE BEGIN                                               
  thm_load_state, probe=sc, datatype='spinper'           
  get_data, SpinName[0], data=SpinPer   
  IF data_type(SpinPer) NE 8 THEN BEGIN
    print, 'THM_EFI_CLEAN_EFP: Cannot get spin period. Exiting...'
    return
  ENDIF
ENDELSE                                                  

; GET ELECTRON TEMPERATURE DATA 
IF keyword_set(use_electrons) then BEGIN
  if not keyword_set(TeName) then TeName = 'th' + sc + '_peeb_avgtemp'
  IF thm_check_tvar(TeName) then BEGIN                          
    get_data, TeName[0], data=peeb_t       
  ENDIF ELSE BEGIN                                               
    print, 'THM_EFI_CLEAN_EFP: Electron temperature data not stored. Fetching...'
    thm_load_esa, probe=sc, datatype = 'peeb_avgtemp', level = 'l2', $
      /get_support, trange = trange
    TeName = 'th' + sc + '_peeb_avgtemp'
    get_data, TeName[0], data=peeb_t
    IF data_type(peeb_t) NE 8 then BEGIN
      print, 'THM_EFI_CLEAN_EFP: Cannot get electron temperature. Not using.'
      use_electrons = 0
    ENDIF
  ENDELSE                                                  
ENDIF

; CLIP DATA TO RANGE
IF keyword_set(trange) then BEGIN
   trange_clip, E, trange(0), trange(1), /data, BadClip=BadEclip
   trange_clip, Vsc, trange(0), trange(1), /data, BadClip=BadVclip
   trange_clip, Bdsl, trange(0), trange(1), /data, BadClip=BadBclip
   trange_clip, SpinPer, trange(0)-60.d, trange(1)+60.d, /data, BadClip=BadSclip
   if data_type(peeb_t) eq 8 then trange_clip, peeb_t, trange(0), trange(1), /data, BadClip=BadPclip
   IF (keyword_set(BadEclip) OR keyword_set(BadVclip) OR keyword_set(BadBclip) OR $
       keyword_set(BadSclip) OR keyword_set(BadPclip) ) THEN BEGIN
     print, 'THM_EFI_CLEAN_EFP: Problem with trange clip. Exiting...'
     print, '0=OK; 1=Problem. E:', BadEclip, 'V:', BadVclip, 'B:', BadBclip, 'Spin:', BadSclip
     return
   ENDIF
ENDIF

; CREATE ARRAYS FOR OUTPUT
EderSave  = E.y(*,0) * 0

; ## IDENTIFY INDIVIDUAL PARTICLE BURSTS  ##
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
    print, 'THM_EFI_CLEAN_EFP: Spin period missing during burst. Exiting...'
    return
  ENDIF
  per = median(spinper.y(ind))
   
  ; GET SC POTENTIAL
  VscTemp = Vsc
  trange_clip, VscTemp, Ttemp(0), Ttemp(1), /data
  VscF   = thm_lsp_remove_spintone(VscTemp.x, VscTemp.y, per, talk=talk, ns=SpinNsmooth, sp = spinpoly)
  VscF   = thm_lsp_remove_spintone(VscTemp.x, VscF, per/2,    talk=talk, ns=SpinNsmooth, sp = spinpoly)
  VscF   = thm_lsp_remove_spintone(VscTemp.x, VscF, per/4,    talk=talk, ns=SpinNsmooth, sp = spinpoly)

  ; REMOVE SPIN TONES FROM E
  IF keyword_set(SpinRemove) then BEGIN
    Exf = thm_lsp_remove_spintone(t, Ex, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    Eyf = thm_lsp_remove_spintone(t, Ey, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    Ezf = thm_lsp_remove_spintone(t, Ez, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    IF ( (SpinRemove EQ 2) OR (SpinRemove GE 4) ) then BEGIN
      Exf = thm_lsp_remove_spintone(t, Exf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Eyf = thm_lsp_remove_spintone(t, Eyf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Ezf = thm_lsp_remove_spintone(t, Ezf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
    ENDIF
    IF (SpinRemove GE 3) then BEGIN
      Exf = thm_lsp_remove_spintone(t, Exf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Eyf = thm_lsp_remove_spintone(t, Eyf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Ezf = thm_lsp_remove_spintone(t, Ezf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
    ENDIF
  ENDIF

  ; REMOVE POTENTIAL FROM Ez
  if keyword_set(VscRemove) then $
    Ezf = thm_lsp_remove_potential(t, Ezf, VscTemp.x, VscF, peeb_t=peeb_t, $
                                   VscPole=VscPole, Vpoly=Vpoly, talk=talk)

  ; REMOVE SPIKES
  IF keyword_set(SpikeRemove) then BEGIN
    thm_lsp_remove_spikes, t, Exf, Eyf, Ezf, per, Nwin=SpikeNwin, $
              SpikeSig=SpikeSig, Sigmin=SpikeSigmin, Nfit=SpikeNfit, Fit=SpikeFit, $
              talk=talk, diagnose=diagnose, wt=wt
  ENDIF
  
  ; REMOVE SPIN TONES FROM E
  IF keyword_set(SpinRemove) then BEGIN
    Exf = thm_lsp_remove_spintone(t, Exf, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    Eyf = thm_lsp_remove_spintone(t, Eyf, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    Ezf = thm_lsp_remove_spintone(t, Ezf, per,    talk=talk, ns=SpinNsmooth, sp=spinpoly)
    IF ( (SpinRemove EQ 2) OR (SpinRemove GE 4) ) then BEGIN
      Exf = thm_lsp_remove_spintone(t, Exf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Eyf = thm_lsp_remove_spintone(t, Eyf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Ezf = thm_lsp_remove_spintone(t, Ezf, per/2, talk=talk, ns=SpinNsmooth, sp=spinpoly)
    ENDIF
    IF (SpinRemove GE 3) then BEGIN
      Exf = thm_lsp_remove_spintone(t, Exf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Eyf = thm_lsp_remove_spintone(t, Eyf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
      Ezf = thm_lsp_remove_spintone(t, Ezf, per/4, talk=talk, ns=SpinNsmooth, sp=spinpoly)
    ENDIF
  ENDIF

  ; REMOVE EPOCH
  IF keyword_set(EpochRemove) then BEGIN
    Exf = thm_lsp_remove_spin_epoch(t, Exf, per, talk=talk)
    Eyf = thm_lsp_remove_spin_epoch(t, Eyf, per, talk=talk)
    Ezf = thm_lsp_remove_spin_epoch(t, Ezf, per, talk=talk)
  ENDIF

  ; FIT AXIAL TO EDERIVED
  IF keyword_set(FixAxial) then BEGIN
    Btemp = Bdsl
    Etemp = E
    trange_clip, Etemp, Ttemp(0), Ttemp(1), /data
    trange_clip, Btemp, Ttemp(0), Ttemp(1), /data
    Etemp.y(*,0) = Exf
    Etemp.y(*,1) = Eyf
    Etemp.y(*,2) = Ezf
    Eder = thm_lsp_derive_Ez(Etemp, Btemp, minBz=minBz, minRat=MinRatio, ratio=ratio)
    Ezf  = thm_lsp_fix_axial(t, Ezf, Eder, talk=talk, AxPoly=Axpoly, soft=softmerge,  $
                             Fmerge=Fmerge, MergeRatio=MergeRatio, ratio=ratio)
  ENDIF
  
  ; SAVE E DATA
  E.y(bstart(ib):bend(ib),0)     = Exf
  E.y(bstart(ib):bend(ib),1)     = Eyf
  E.y(bstart(ib):bend(ib),2)     = Ezf
  EderSave(bstart(ib):bend(ib))  = Eder

  ; FIX B SPINTONE
  magstart = value_locate(Bdsl.x, Btemp.x(0)+1.d-8)
  magstop  = magstart + n_elements(Btemp.x) - 1L
  IF BspinPoly GE 0 then BEGIN
    Btemp.y(*,0) = thm_lsp_remove_spintone(Btemp.x, Btemp.y(*,0), per, talk=talk, spinpoly=Bspinpoly)
    Btemp.y(*,1) = thm_lsp_remove_spintone(Btemp.x, Btemp.y(*,1), per, talk=talk, spinpoly=Bspinpoly)
    Btemp.y(*,2) = thm_lsp_remove_spintone(Btemp.x, Btemp.y(*,2), per, talk=talk, spinpoly=Bspinpoly)
  ENDIF
  
  ; SMOOTH B
  IF Bsmooth GT 1 THEN BEGIN
    Btemp.y(*,0) = smooth(Btemp.y(*,0), Bsmooth, /nan, /edge)
    Btemp.y(*,1) = smooth(Btemp.y(*,1), Bsmooth, /nan, /edge)
    Btemp.y(*,2) = smooth(Btemp.y(*,2), Bsmooth, /nan, /edge)
  ENDIF
  
  ; SAVE B DATA
  Bdsl.y(magstart:magstop, 0) =  Btemp.y(*,0)
  Bdsl.y(magstart:magstop, 1) =  Btemp.y(*,1)
  Bdsl.y(magstart:magstop, 2) =  Btemp.y(*,2)

ENDFOR
; ## END OF LOOP

; ### STORE DATA AS TPLOT VARIABLES

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
            BAND: 'DC - ~50 Hz'}   ; BAND - the freq band of the data

; STORE E DATA
ename2 = ename + '_clean_dsl'
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: elim.colors, $
  LABELS: ['Ex', 'Ey', 'Ez'], LABFLAG: elim.labflag, $
  YTITLE: 'E_DSL (th' + sc +')'}
store_data, ename2[0], data=E, dlim=dlim

; STORE B DATA
bname2 = Bdslname + '_smooth'
store_data, bname2[0], data=Bdsl, dlim=blim

; STORE Ederived DATA
Edername = ename + '_derived'
Etemp = dblarr(n_elements(E.x),4)
Etemp(*,0) = E.y(*,0)
Etemp(*,1) = E.y(*,1)
Etemp(*,2) = E.y(*,2)
Etemp(*,3) = EderSave
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: [elim.colors, 0], $
  LABELS: ['Ex', 'Ey', 'Ez', 'E!Dzder!N'], LABFLAG: elim.labflag, $
  YTITLE: 'E - ' + data_att.coord_sys}
store_data, Edername[0], data={X:E.x, Y: Etemp, V: [1,2,3,4]}, dlim=dlim

; #### COORDINATE TRANSFORMATIONS

; TRANSFORM TO GSM
egsmname = ename + '_clean_gsm'
thm_cotrans, ename2, egsmname, in_coord='dsl', out_coord='gsm'
get_data, egsmname[0], data = data
data_att.coord_sys = 'gsm'
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: elim.colors, $
  LABELS: ['Ex', 'Ey', 'Ez'], LABFLAG: elim.labflag, $
  YTITLE: 'E_GSM (th' + sc +')'}
store_data, egsmname[0], data = data, dlim = dlim

; GO TO FAC COORDINATES (JIANBAO)
efacname = ename + '_clean_fac'
thm_fac_matrix_make, bname2, other_dim='zdsl', newname='th'+sc+'_fgh_fac_mat'
tvector_rotate, 'th'+sc+'_fgh_fac_mat', ename2, $
          newname=efacname, error=error
get_data, efacname[0], data = data
perp1 = 'E!DSP!N'
perp2 = 'E!Dperp!N'
para = 'E!D||!N'
data_att.coord_sys = 'fac: x in spin-plane'
dlim = {CDF: elim.cdf, SPEC: 0b, LOG: 0b, YSUBTITLE: '(mV/m)', $
  DATA_ATT: data_att, COLORS: elim.colors, $
  LABELS: [perp1, perp2, para], LABFLAG: elim.labflag, $
  YTITLE: 'E_FAC (th' + sc +')'}
store_data, efacname[0], data = data, dlim = dlim


end
