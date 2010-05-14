;+
;NAME:
; thm_data2load
;PURPOSE:
; returns a list of variables that can be loaded for a given THEMIS
; instrument. For the most part, it calls the appropriate thm_load
; routine with the /valid_names keyword set. The dataypes and
; valid_names keywords are used inconsistently in the thm_loads, and
; do not allow for the distinction between level2 data that is
; to be input from level2 files, and level2 data the is to be input
; from level1 files and calbrated, cotrans'ed, etc... but has the same
; name as an L2 variable. Designed to be called from
; thm_ui_valid_dtype.pro
;CALLING SEQUENCE:
; dtyp = thm_data2load(instrument, level)
;INPUT:
; instrument = the THEMIS instrument: one of:
;           ['asi', 'ask', 'esa', 'efi', 'fbk', 'fft', 'fgm', $
;            'fit', 'gmag', 'mom', 'scm', 'sst', 'state', 'bau', 'hsk', 'trg']
; level = 'l1' for any data that can be gotten from the l1 file --
;         including calibrated, etc... 'l2' for data gotten from L2
;         files. 'l10' for data that only is loaded from L1 files. For
;         ESA data, 'L10' data and 'L1' data are gotten from the
;         packet files.
;OUTPUT:
; dtyp = a string array that can be used as an input to the datatype
;        keyword for the given instrument
;HISTORY:
; started on 31-Jan-2008, jmm, jimm@ssl.berkeley.edu, this is under
; development for the next 6 months or so.
; 9-apr-2008, jmm, added all instruments, for Version 4_00
;$LastChangedBy: jwl $
;$LastChangedDate: 2010-03-05 15:59:14 -0800 (Fri, 05 Mar 2010) $
;$LastChangedRevision: 7402 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/common/thm_data2load.pro $
;-
Function thm_data2load, instrument, level

  instr = strcompress(strlowcase(instrument), /remove_all)
  lvl = strcompress(strlowcase(level), /remove_all)

  instr0 = ['asi', 'ask', 'esa', 'efi', 'fbk', 'fft', 'fgm', $
            'fit', 'gmag', 'mom', 'scm', 'spin', 'sst', 'state', 'bau', 'hsk', 'trg']
  lvl0 = ['l1', 'l2', 'l10']

  xx = where(instr0 Eq instr)
  If(xx[0] Eq -1) Then Begin
    message, /info, 'Invalid Input: '+instrument
    print, 'Try, doc_library, ''thm_data2load'''
    return, ''
  Endif
  xx = where(lvl0 Eq lvl)
  If(xx[0] Eq -1) Then Begin
    message, /info, 'Invalid Input: '+level
    print, 'Try, doc_library, ''thm_data2load'''
    return, ''
  Endif
;special cases first, eventually all will be special cases to
;distinguish between l1, and l10
  Case instr Of
    'asi':Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
        vvv = ['asf', 'ast']
      Endif Else Begin
        vvv = ''
      Endelse
    End
    'ask':Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
        vvv = 'ask'
      Endif Else Begin
        vvv = ''
      Endelse
    End
    'esa': Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
        vvv = ['peif', 'peir', 'peib', 'peef', 'peer', 'peeb']
      Endif Else Begin
        ii = ['peif','peir','peib','peef','peer','peeb']
        vv = ['mode', 'en_eflux', 'sc_pot', 'magf', $
              'density', 'avgtemp', 'vthermal', 'flux', $
              'ptens', 'mftens', 't3', 'symm', 'symm_ang', $
              'magt3', 'velocity_dsl', 'velocity_gse', $
              'velocity_gsm', 'data_quality']
        vvv = ''
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
        vvv = vvv[1:*]
      Endelse
    End
    'efi':Begin
      If(lvl Eq 'l10') Then Begin
        vvv = ['vaf', 'vap', 'vaw', 'vbf', 'vbp', 'vbw', 'eff', $
               'efp', 'efw', 'eff_0', 'efp_0', 'efw_0', 'eff_dot0', $
               'efp_dot0', 'efw_dot0']
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ['vaf', 'vap', 'vaw', 'vbf', 'vbp', 'vbw', 'eff', $
               'efp', 'efw', 'eff_0', 'efp_0', 'efw_0', 'eff_dot0', $
               'efp_dot0', 'efw_dot0']
      Endif Else Begin
        vvv = ['eff_dot0', 'efs_dot0']
      Endelse
    End
    'fbk':Begin
      If(lvl Eq 'l10') Then Begin
        vvv = ['fbh', 'fb1', 'fb1_src', 'fb2', 'fb2_src']
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ['fb1', 'fb2', 'fb_eac12', 'fb_eac34', 'fb_eac56', 'fb_edc12', $
               'fb_edc34', 'fb_edc56', 'fb_hff', 'fb_scm1', 'fb_scm2', $
               'fb_scm3', 'fb_v1', 'fb_v2', 'fb_v3', 'fb_v4', 'fb_v5', $
               'fb_v6', 'fbh']
      Endif Else Begin
        vvv = ['fb_v1', 'fb_v2', 'fb_v3', 'fb_v4', 'fb_v5', 'fb_v6', $
               'fb_edc12', 'fb_edc34', 'fb_edc56', 'fb_scm1', 'fb_scm2', $
               'fb_scm3', 'fb_eac12', 'fb_eac34', 'fb_eac56', 'fb_hff']
      Endelse
    End
    'fft':Begin
      If(lvl Eq 'l10') Then Begin
        vvv = ''
        ii = ['ffp_16', 'ffw_16', 'fff_16', 'ffp_32', 'ffw_32', 'fff_32', 'ffp_64', 'ffw_64', 'fff_64']
        vv = ['src', 'adc', 'hed']
        vvv = ii
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ''
        ii = ['ffp_16', 'ffw_16', 'fff_16', 'ffp_32', 'ffw_32', 'fff_32', 'ffp_64', 'ffw_64', 'fff_64']
        vv = ['dbpara', 'dbperp', 'eac12', 'eac34', 'eac56', 'edc12', $
              'edc34', 'edc56', 'epara', 'eperp', 'scm1', 'scm2', 'scm3', $
              'undef', 'v1', 'v2', 'v3', 'v4', 'v5', 'v6','src','adc','hed']
        vvv = ii
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
      Endif Else Begin
        vvv = ''
      Endelse
    End
    'fgm':Begin
      If(lvl Eq 'l10') Then Begin
        vvv = ['fge', 'fgl', 'fgh']
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ['fge', 'fgl', 'fgh']
      Endif Else Begin
        vl2_coord = '_'+['ssl', 'dsl', 'gse', 'gsm', 'btotal'] ;yes, btotal isn't a coordinate system
        vl2_coord_fgs = '_'+['dsl', 'gse', 'gsm', 'btotal']
        vvv = ['fge'+vl2_coord, 'fgl'+vl2_coord, 'fgh'+vl2_coord, 'fgs'+vl2_coord_fgs]
      Endelse
    End
    'fit': Begin
      If(lvl Eq 'l10') Then Begin
        vvv = 'fit'
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ['fit', 'fit_bfit', 'fit_efit', 'fgs', 'fgs_sigma', $
               'efs', 'efs_0', 'efs_dot0', 'efs_sigma']
      Endif Else Begin
        vl2_coord = '_'+['dsl', 'gse', 'gsm']
        vl2 = ['fgs'+vl2_coord, 'efs'+vl2_coord, $
               'efs_0'+vl2_coord, 'efs_dot0'+vl2_coord]
        vvv = [vl2, 'fit_efit', 'fit_bfit', 'fgs_sigma', 'efs_sigma']
      Endelse
    End
    'gmag': Begin
      If(lvl Eq 'l2') Then Begin
        vvv = 'mag'
      Endif Else Begin
        vvv = ''
      Endelse
    End
    'mom': Begin
;      ii = ['peim', 'peem', 'psim', 'psem', 'ptim', 'ptem']
      ii = ['peim', 'peem']
      vvv = ''
      If(lvl Eq 'l10' Or lvl Eq 'l1') Then Begin ;for 'l10' set the raw keyword
        vv = ['density', 'flux', 'mftens', 'eflux']+'_raw'
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
        vvv = [vvv[1:*], 'pxxm_pot_raw', 'pxxm_qf', 'pxxm_shft']
;      Endif Else If(lvl Eq 'l1') Then Begin
;        vv = ['density', 'flux', 'mftens', 'eflux', 'velocity', $
;        'ptens', 'ptot', 'velocity_mag', 'ptens_mag', 't3_mag', 'mag']
;        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
;        vvv = [vvv[1:*], 'pxxm_pot', 'pxxm_qf', 'pxxm_shft']
      Endif Else Begin
        vv = ['density', 'flux', 'mftens', 'eflux', 'velocity_dsl', $
        'ptens', 'ptot', 'velocity_mag', 'ptens_mag', 't3_mag', $
        'mag', 'velocity_gse', 'velocity_gsm', 'data_quality']
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
        vvv = [vvv[1:*], 'pxxm_pot']
      Endelse
    End
    'scm': Begin
      If(lvl Eq 'l10') Then Begin
        vvv = ['scf', 'scp', 'scw']
      Endif Else If(lvl Eq 'l1') Then Begin
        vvv = ['scf', 'scp', 'scw']
      Endif Else if (lvl eq 'l2') then begin
        ;currently no valid level 2 scm type
        vvv = ''
      endif else begin
        vvv = ['scf', 'scp', 'scw']
      Endelse
    End
    'spin': Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
       vvv = 'spin_'+['spinper', 'tend', 'c', 'phaserr', 'nspins', 'npts', 'maxgap']
      Endif Else Begin
        vvv = ''
      Endelse
    End
    'sst': Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
        ii = ['psif', 'psef', 'psir', 'pser']
        vv = ['ang', 'en', 'tot']
        vvv = ''
        For k = 0, n_elements(ii)-1 Do vvv = [vvv, ii[k]+'_'+vv]
        vvv = [vvv[1:3], 'psif_cnfg', 'psif_nspn', vvv[4:*]]
      Endif Else Begin
        vvv = ['psif', 'psef']+'_en_eflux'
      Endelse
    End
    'state': Begin
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
       vvv = ['pos', 'vel', 'man', 'roi', 'spinras', 'spindec', $
              'spinalpha', 'spinbeta', 'spinper', 'spinphase', $
              'spin_spinper',  'spin_tend', 'spin_c', $
              'spin_phaserr', 'spin_nspins', 'spin_npts', 'spin_maxgap','spinras_correction','spindec_correction','spinras_corrected','spindec_corrected']
      Endif Else Begin
        vvv = ''
      Endelse
    End
    Else: Begin
      load_routine = 'thm_load_'+instr
      resolve_routine, load_routine, /no_recompile
      If(lvl Eq 'l1' Or lvl Eq 'l10') Then Begin
        call_procedure, load_routine, level = 'l1', /valid_names, datatype = vvv
      Endif Else Begin
        call_procedure, load_routine, level = 'l2', /valid_names, datatype = vvv
      Endelse
    End
  Endcase
  Return, vvv
End

        

      
 
