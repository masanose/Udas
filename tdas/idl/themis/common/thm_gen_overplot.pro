;+
; Purpose: To make mission overview plots of all instruments
;
; Inputs:  PROBES: spacecraft ('a','b','c','d','e')
;          DATE: the date string or seconds since 1970 ('2007-03-23')
;          DUR: duration (default units are days)
;          DAYS: redundant keyword to set  the units of duration (but its comforting to have)
;          HOURS: keyword to make the duration be in units of hours
;          DEVICE: sets the device (x or z) (default is x)
;          MAKEPNG: keyword to generate 5 png files
;          DIRECTORY: sets the directory where the above pngs are placed (default is './')
;	   FEARLESS: keyword that prevents program from quitting when it fears its in an infinite loop
;			(infinite loop is feared when catch statement has been call 1000 times)
;	   DONT_DELETE_DATA:  keyword to not delete all existing tplot variables before loading data in for
;			        the overview plot (sometimes old variables can interfere with overview plot)
;
; Example: thm_gen_overplot,probe='a',date='2007-03-23',dur=1
;	   The above example will produce a full day plot in the X window.
;
;
;Version:
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

pro thm_gen_overplot, probes = probes, date = date, dur = dur, $
                      days = days, hours = hours, device = device, $
                      directory = directory, makepng = makepng, $
                      fearless = fearless, dont_delete_data = dont_delete_data

;catch errors and fail gracefully
;-------------------------------------------------------


common overplot_position, load_position, error_count

!quiet=0
error_count=0
load_position = 'init'

;catch statement to allow program to recover from errors
;-------------------------------------------------------

catch,error_status

if error_status ne 0 then begin

   error_count++
   if error_count ge 1000. and not keyword_set(fearless) then begin
     print, ' '
     print, 'The program is quitting because it fears its in an infinite loop.'
     print, 'To eliminate this fear add the keyword /fearless to the call.'
     return
   endif

   print, '***********Catch error**************'
   help, /last_message, output = err_msg
   For j = 0, n_elements(err_msg)-1 Do print, err_msg[j]
   print, 'load_position: ' , load_position

   case load_position of
        'init'          : goto, SKIP_DAY
   	'fgm'		: goto, SKIP_FGM_LOAD
   	'fbk'		: goto, SKIP_FBK_LOAD
   	'sst'		: goto, SKIP_SST_LOAD
   	'esa'		: goto, SKIP_ESA_LOAD
   	'gmag'		: goto, SKIP_GMAG_LOAD
   	'asi'		: goto, SKIP_ASI_LOAD
   	'pos'		: goto, SKIP_POS_LOAD
   	'mode'		: goto, SKIP_SURVEY_MODE
   	'bound'		: goto, SKIP_BOUNDS
   	else		: goto, SKIP_DAY

  endcase

endif

;check some inputs
;-------------------------------------------------------
if keyword_set(probes) then sc = strlowcase(probes) ; quick change of variable name

vsc = ['a','b','c','d','e']
if not keyword_set(sc) then begin
  print,'You did not enter a spacecraft into the program call.'
  print, "Valid inputs are: 'a','b','c','d','e'  (ie, sc='b')"
  return
endif
if total(strmatch(vsc,strtrim(strlowcase(sc)))) gt 1 then begin
  print, 'This program is only designed to accept a single spacecraft as input.'
  print, "Valid inputs are: 'a','b','c','d','e'  (ie, sc='b')"
  return
endif
if total(strmatch(vsc,strtrim(strlowcase(sc)))) eq 0 then begin
  print,"The input sc= '",strtrim(sc),"' is not a valid input."
  print, "Valid inputs are: 'a','b','c','d','e'  (ie, sc='b')"
  return
endif

if ~keyword_set(dur) then begin
  message, /info, 'duration not input, setting dur = 1'
  dur = 1
endif
if (dur Lt 0) then begin
  message, /info, 'Invalid duration, setting dur = 1'
  dur = 1
endif

if keyword_set(hours) then dur=dur/24.

if not keyword_set(date) then begin
  print,'You did not enter a date into the program call.'
  print,"Example: thm_gen_overplot,sc='b',date='2007-03-23'"
  return
endif else begin
  t0 = time_double(date)
  t1 = t0+dur*60D*60D*24D
  
  if t1 lt time_double('2007-02-17/00:00:00') then begin
    print, 'Invalid time entered: ', date
    return
  endif else if (t0 Gt systime(/seconds)) then begin
    print, 'Invalid time entered: ', date
    return
  endif
endelse


valid_devices = ['cgm','hp','metafile','null','pcl','printer','ps','regis','tek','win','x','z']

if ~keyword_set(device) then begin
  help,/device,output=plot_device
  plot_device=strtrim(strlowcase(strmid(plot_device[1],24)),2)
  if plot_device eq 'z' then device, set_resolution = [750, 800]
endif else begin

  if ~in_set(strlowcase(device),valid_devices) then begin
    message,/info,'Device keyword has invalid value. Returning'
    return
  endif  

  set_plot,device
  help,/device,output=plot_device
  plot_device=strtrim(strlowcase(strmid(plot_device[1],24)),2)
  if plot_device eq 'z' then device, set_resolution = [750, 800]
endelse

thx = 'th'+sc[0]                ;need a scalar for this

date=time_string(date)

if not keyword_set(dont_delete_data) then begin
  del_data, '*'                 ; give ourselves a clean slate
  clear_esa_common_blocks
  common data_cache_com, dcache
  dcache = ''
endif

timespan,date,dur

;load magnetic field fit data
;-----------------------------

load_position='fgm'

thm_load_state,probe=sc,/get_support
thm_load_fit,lev=1,probe=sc,/get_support

SKIP_FGM_LOAD:

;kluge to prevent missing data from crashing things
index_fit=where(thx+'_fgs' eq tnames())
index_state=where(thx+'_state_spinras' eq tnames())
if (index_fit[0] eq -1 or index_state[0] eq -1) then begin
  filler=fltarr(2,3)
  filler[*,*]=float('NaN')
  store_data,thx+'_fgs_gse',data={x:time_double(date)+findgen(2),y:filler}
  ylim,thx+'_fgs_gse',-100,100,0
endif else begin
  thm_cotrans,thx+'_fgs',out_suf='_gse', in_c='dsl', out_c='gse'
endelse

;clip data
tclip, thx+'_fgs_gse', -100.0, 100.0, /overwrite
name = thx+'_fgs_gse'
options, name, 'ytitle', 'B FIT!CGSE (nT)'
options, name, 'labels', ['Bx', 'By', 'Bz']
options, name, 'labflag', 1
options, name, 'colors', [2, 4, 6]


;load FBK data
;--------------

load_position='fbk'

thm_load_fbk,lev=1,probe=sc

SKIP_FBK_LOAD:

del_data,thx+'_fb_h*'  ; del thx_fb_hff as it is not used and gets in the way later
fbk_tvars=tnames(thx+'_fb_*') ; this should give us two tplot variables (but sometimes more)

if n_elements(fbk_tvars) eq 1 then fbk_tvars=[fbk_tvars,'filler']

for i=0,n_elements(fbk_tvars)-1 do begin
  ;kluge to prevent missing data from crashing things
  get_data,fbk_tvars[i],data=dd
  if size(dd,/type) ne 8 then begin
    filler = fltarr(2, 6)
    filler[*, *] = float('NaN')
    name = thx+'_fb_'+strcompress(string(i+1), /remove_all)
    store_data, name, data = {x:time_double(date)+findgen(2), y:filler, v:findgen(6)}
    options, name, 'spec', 1
    ylim, name, 1, 1000, 1
    zlim, name, 0, 0, 1
  endif else begin
;    store_data,fbk_tvars(i),data={x:dd.x,y:dd.y,v:[2.,8.,32.,128.,512.,2048.]}
    store_data, fbk_tvars[i], data = {x:dd.x, y:dd.y, v:[2048., 512., 128., 32., 8., 2.]}
    options, fbk_tvars[i], 'spec', 1
    options, fbk_tvars[i], 'zlog', 1
    ylim, fbk_tvars[i], 2.0, 2048.0, 1
    thm_spec_lim4overplot, fbk_tvars[i], ylog = 1, zlog = 1, /overwrite
    options, fbk_tvars[i], 'ytitle', thx+'!CFBK '+strmid(fbk_tvars[i], 7)
;for ztitle, we need to figure out which type of data is there
;      for V channels, <|V|>.
;      for E channels, <|mV/m|>.
;      for SCM channels, <|nT|>.
    x1 = strpos(fbk_tvars[i], 'scm')
    If(x1[0] Ne -1) Then Begin
      options, fbk_tvars[i], 'ztitle', '<|nT|>'
;reset the upper value of zlimit to 2.0, jmm, 30-nov-2007
      get_data, fbk_tvars[i], data = d
      If(is_struct(d)) Then zlim,  fbk_tvars[i], min(d.y), 2.0, 1
    Endif
    xv = strpos(fbk_tvars[i], 'v')
    If(xv[0] Ne -1) Then options, fbk_tvars[i], 'ztitle', '<|V|>'
    xe = strpos(fbk_tvars[i], 'e')
    If(xe[0] Ne -1) Then Begin
      options, fbk_tvars[i], 'ztitle', '<|mV/m|>'
;reset the upper value of zlimit to 2.0, jmm, 30-nov-2007
      get_data, fbk_tvars[i], data = d
      If(is_struct(d)) Then zlim,  fbk_tvars[i], min(d.y), 2.0, 1
    Endif
  endelse

endfor

;load SST spectrograms
;----------------------

load_position='sst'
thm_load_sst, probe = sc, level = 'l2'
;If Level 2 data didn't show up, check for L1
index_sst_e = where(thx+'_psef_en_eflux' eq tnames())
index_sst_i = where(thx+'_psif_en_eflux' eq tnames())
if(index_sst_e[0] eq -1 Or index_sst_i[0] Eq -1) then begin
  thm_load_sst, probe = sc, level = 'l1'
  thm_part_moments, probe = sc, instrument = ['psif', 'psef'], $
    moments = ['density', 'velocity', 't3']
endif

SKIP_SST_LOAD:
;kluge to prevent missing data from crashing things
index_sst=where(thx+'_psif_en_eflux' eq tnames())
if index_sst eq -1 then begin
  filler = fltarr(2, 16)
  filler[*,*]=float('NaN')
  store_data, thx+'_psif_en_eflux', $
    data = {x:time_double(date)+findgen(2)*86400., y:filler, v:findgen(16)}
  name = thx+'_psif_en_eflux'
  options, name, 'spec', 1
  ylim, name, 1, 1000, 1
  zlim, name, 1d1, 5d2, 1
  options, name, 'ytitle', thx+'!CSST ions!CeV'
  options, name, 'ysubtitle', ''
;  options, name, 'ztitle', 'Eflux !C eV/cm!U2!N!C-s-sr-eV'
  options, name, 'ztitle', 'Eflux, EFU'
endif else begin
;SST ion panel
  name = thx+'_psif_en_eflux'
  tdegap, name, /overwrite, dt = 600.0
  options, name, 'spec', 1
  options, name, 'ytitle', thx+'!CSST ions!CeV'
  options, name, 'ysubtitle', ''
;  options, name, 'ztitle', 'Eflux !C eV/cm!U2!N!C-s-sr-eV'
  options, name, 'ztitle', 'Eflux, EFU'
  options, name, 'y_no_interp', 1
  options, name, 'x_no_interp', 1
  zlim, name, 1d1, 5d2, 1
endelse
index_sst = where(thx+'_psef_en_eflux' eq tnames())
if index_sst eq -1 then begin
  filler = fltarr(2, 16)
  filler[*, *] = float('NaN')
  store_data, thx+'_psef_en_eflux', $
    data = {x:time_double(date)+findgen(2), y:filler, v:findgen(16)}
  name = thx+'_psef_en_eflux'
  options, name, 'spec', 1
  ylim, name, 1, 1000, 1
  zlim, name, 1d1, 5d2, 1
  options, name, 'ytitle', thx+'!CSST elec!CeV'
  options, name, 'ysubtitle', ''
;  options, name, 'ztitle', 'Eflux !C eV/cm!U2!N!C-s-sr-eV'
  options, name, 'ztitle', 'Eflux, EFU'
endif else begin
;SST electron panel
  name = thx+'_psef_en_eflux'
  tdegap, name, /overwrite, dt = 600.0
  options, name, 'spec', 1
  options, name, 'ytitle', thx+'!CSST elec!CeV'
  options, name, 'ysubtitle', ''
;  options, name, 'ztitle', 'Eflux !C eV/cm!U2!N!C-s-sr-eV'
  options, name, 'ztitle', 'Eflux, EFU'
  options, name, 'y_no_interp', 1
  options, name, 'x_no_interp', 1
  zlim, name, 1d1, 5d2, 1
endelse

;load ESA spectrograms and moments
;----------------------------------
load_position='esa'
;load both full and reduced data:
mtyp = ['f', 'r']
ok_esai_flux = bytarr(2)
ok_esae_flux = bytarr(2)
ok_esai_moms = bytarr(2)
ok_esae_moms = bytarr(2)
For j = 0, 1 Do Begin
  thm_load_esa, probe = sc, datatype = 'pe?'+mtyp[j]+'*', level = 'l2'
  itest = thx+'_pei'+mtyp[j]
  etest = thx+'_pee'+mtyp[j]
;If Level 2 data didn't show up, check for L1
  index_esa_e_en = where(etest+'_en_eflux' eq tnames())
  index_esa_e_d = where(etest+'_density' eq tnames())
  index_esa_e_v = where(etest+'_velocity_dsl' eq tnames())
  index_esa_e_t = where(etest+'_t3' eq tnames())

  index_esa_i_en = where(itest+'_en_eflux' eq tnames())
  index_esa_i_d = where(itest+'_density' eq tnames())
  index_esa_i_v = where(itest+'_velocity_dsl' eq tnames())
  index_esa_i_t = where(itest+'_t3' eq tnames())

  if(index_esa_e_en[0] eq -1 Or index_esa_i_en[0] Eq -1) then begin
    thm_load_esa_pkt, probe = sc
    instr_all = ['pei'+mtyp[j], 'pee'+mtyp[j]]
    for k = 0, 1 do begin
      test_index = where(thx+'_'+instr_all[k]+'_en_counts' eq tnames())
      If(test_index[0] Ne -1) Then Begin
        thm_part_moments, probe = sc, instrument = instr_all[k], $
          moments = '*'
        copy_data, thx+'_'+instr_all[k]+'_velocity', $
          thx+'_'+instr_all[k]+'_velocity_dsl'
     Endif
    endfor
    index_esa_e_en = where(etest+'_en_eflux' eq tnames())
    index_esa_e_d = where(etest+'_density' eq tnames())
    index_esa_e_v = where(etest+'_velocity_dsl' eq tnames())
    index_esa_e_t = where(etest+'_t3' eq tnames())
    
    index_esa_i_en = where(itest+'_en_eflux' eq tnames())
    index_esa_i_d = where(itest+'_density' eq tnames())
    index_esa_i_v = where(itest+'_velocity_dsl' eq tnames())
    index_esa_i_t = where(itest+'_t3' eq tnames())
  endif
  if index_esa_i_en[0] eq -1 then begin
    filler = fltarr(2, 32)
    filler[*, *] = float('Nan')
    name1 = itest+'_en_eflux'
    store_data, name1, data = {x:time_double(date)+findgen(2), y:filler, v:findgen(32)}
    zlim, name1, 1d3, 7.5d8, 1
    ylim, name1, 3., 40000., 1
;    options, name1, 'ztitle', 'Eflux !C!C eV/cm!U2!N!C-s-sr-eV'
    options, name1, 'ztitle', 'Eflux, EFU'
    options, name1, 'ytitle', 'ESA i+ '+thx+'!C eV'
    options, name1, 'ysubtitle', ''
    options, name1, 'spec', 1
  endif else begin
    name1 = itest+'_en_eflux'
    tdegap, name1, /overwrite, dt = 600.0
    zlim, name1, 1d3, 7.5d8, 1
    ylim, name1, 3., 40000., 1
;    options, name1, 'ztitle', 'Eflux !C!C eV/cm!U2!N!C-s-sr-eV'
    options, name1, 'ztitle', 'Eflux, EFU'
    options, name1, 'ytitle', 'ESA i+ '+thx+'!C eV'
    options, name1, 'ysubtitle', ''
    options, name1, 'spec', 1
    options, name1, 'x_no_interp', 1
    options, name1, 'y_no_interp', 1
    ok_esai_flux[j] = 1
  endelse

  if index_esa_i_d[0] eq -1 then begin
    filler = fltarr(2)
    filler[*] = float('Nan')
    store_data, itest+'_density', data = {x:time_double(date)+findgen(2), y:filler}
;    options, itest+'_density', 'ytitle', 'Ni '+thx+'!C!C1/cm!U3'
    options, itest+'_density', 'ytitle', 'Ni '+thx
  endif else begin
    name1 = itest+'_density'
    tdegap, name1, /overwrite, dt = 600.0
    ylim, name1, .1, nmax, 1
    options, name1, 'ytitle', 'Ni '+thx
    ok_esai_moms[j] = 1
  endelse

  if index_esa_i_v[0] eq -1 then begin
    filler = fltarr(2, 3)
    filler[*, *] = float('Nan')
    store_data, itest+'_velocity_dsl', data = {x:time_double(date)+findgen(2), y:filler}
    options, itest+'_velocity_dsl', 'ytitle', 'VI '+thx+'!Ckm/s'
    options, itest+'_velocity_dsl', 'ysubtitle', ''
  endif else begin
    name1 = itest+'_velocity_dsl'
    tdegap, name1, /overwrite, dt = 600.0
    itstrg=[t0,t1]
    get_ylimits, name1, itslimits, itstrg
    minmaxvals=itslimits.yrange
    maxvel=max(abs(minmaxvals))
    maxlim=min([maxvel,2000.])
    minlim=0.-maxlim
    if maxvel le 100. then ylim, name1, -50,50,0 else ylim, name1, minlim, maxlim, 0
    options, name1, 'colors', [2, 4, 6]
    options, name1, 'labflag', 1
    options, name1, 'ytitle', 'VI '+thx+'!Ckm/s'
    options, name1, 'ysubtitle', ''
;;    options, name1, labels = ['Vi!dx!n', 'Vi!dy!n', 'Vi!dz!n'], constant = 0.
    options, name1, labels = ['VIx', 'VIy', 'VIz'], constant = 0.
  endelse

  if index_esa_i_t[0] eq -1 then begin
    filler = fltarr(2, 6)
    filler[*, *] = float('Nan')
    store_data, itest+'_t3', data = {x:time_double(date)+findgen(2), y:filler}
    options, itest+'_t3', 'ytitle', 'Ti '+thx+'!CeV'
    options, itest+'_t3', 'ysubtitle', ''
  endif else begin
    name1 = itest+'_t3'
    tdegap, name1, /overwrite, dt = 600.0
    ylim, name1, 10, 10000., 1
    options, name1, 'colors', [2, 4, 6, 0]
    options, name1, 'ytitle', 'Ti '+thx+'!C eV'
    options, name1, 'ysubtitle', ''
  endelse
  
  index_esa_e_en = where(etest+'_en_eflux' eq tnames())
  if index_esa_e_en[0] eq -1 then begin
    filler = fltarr(2, 32)
    filler[*, *] = float('Nan')
    name1 = etest+'_en_eflux'
    store_data, name1, data = {x:time_double(date)+findgen(2), y:filler, v:findgen(32)}
    zlim, name1, 1d4, 7.5d8, 1
    ylim, name1, 3., 40000., 1
;    options, name1, 'ztitle', 'Eflux !C!C eV/cm!U2!N!C-s-sr-eV'
    options, name1, 'ztitle', 'Eflux, EFU'
    options, name1, 'ytitle', 'ESA e- '+thx+'!C eV'
    options, name1, 'ysubtitle', ''
    options, name1, 'spec', 1
  endif else begin 
    name1 = etest+'_en_eflux'
    tdegap, name1, /overwrite, dt = 600.0
    zlim, name1, 1d4, 7.5d8, 1
    ylim, name1, 3., 40000., 1
;    options, name1, 'ztitle', 'Eflux !C!C eV/cm!U2!N!C-s-sr-eV'
    options, name1, 'ztitle', 'Eflux, EFU'
    options, name1, 'ytitle', 'ESA e- '+thx+'!C eV'
    options, name1, 'ysubtitle', ''
    options, name1, 'spec', 1
    options, name1, 'x_no_interp', 1
    options, name1, 'y_no_interp', 1
    ok_esae_flux[j] = 1
  endelse

  if index_esa_e_d[0] eq -1 then begin
    filler = fltarr(2)
    filler[*] = float('Nan')
    store_data, etest+'_density', data = {x:time_double(date)+findgen(2), y:filler}
;    options, etest+'_density', 'ytitle', 'Ne '+thx+'!C!C1/cm!U3'
    options, etest+'_density', 'ytitle', 'Ne '+thx+'!C1/cc'
    options, etest+'_density', 'ysubtitle', ''
no_npot:
    filler = fltarr(2)
    filler[*] = float('Nan')
    store_data, etest+'_density_npot', data = {x:time_double(date)+findgen(2), y:filler}
    options, etest+'_density_npot', 'ytitle', 'Ne '+thx+'!C1/cc'
    options, etest+'_density_npot', 'ysubtitle', ''
  endif else begin 
    name1 = etest+'_density'
    ylim, name1, .1, nmax, 1
;    options, name1, 'ytitle', 'Ne '+thx+'!C!C1/cm!U3'
    options, name1, 'ytitle', 'Ne '+thx+'!C1/cc'
    options, name1, 'ysubtitle', ''
    ok_esae_moms[j] = 1
;Npot calculation, 2009-10-12, jmm
    thm_scpot2dens_opt_n, probe = sc, /no_data_load, datatype_esa = 'pee'+mtyp[j]
;degap after npot calculation
    tdegap, name1, /overwrite, dt = 600.0
    name1x = tnames(etest+'_density_npot')
    get_data, name1x, data = npot_test
    If(is_struct(temporary(npot_test)) Eq 0) Then Goto, no_npot
    tdegap, name1x, /overwrite, dt = 600.0
    options, name1x, 'ytitle', 'Ne '+thx+'!C1/cc'
    options, name1x, 'ysubtitle', ''
  endelse
  
  if index_esa_e_v[0] eq -1 then begin
    filler = fltarr(2, 3)
    filler[*, *] = float('Nan')
    store_data, etest+'_velocity_dsl', data = {x:time_double(date)+findgen(2), y:filler}
;    options, etest+'_velocity_dsl', 'ytitle', 'Ve '+thx+'!C!Ckm/s'
    options, etest+'_velocity_dsl', 'ytitle', 'VE '+thx+'!Ckm/s'
    options, etest+'_velocity_dsl', 'ysubtitle', ''
  endif else begin
    name1 = etest+'_velocity_dsl'
    tdegap, name1, /overwrite, dt = 600.0
    ylim, name1, -500, 200., 0
;    options, name1, 'ytitle', 'Ve '+thx+'!C!Ckm/s'
    options, name1, 'ytitle', 'VE '+thx+'!Ckm/s'
    options, name1, 'ysubtitle', ''
  endelse

  if index_esa_e_t[0] eq -1 then begin
    filler = fltarr(2, 6)
    filler[*, *] = float('Nan')
    store_data, etest+'_t3', data = {x:time_double(date)+findgen(2), y:filler}
    options, etest+'_t3', 'ytitle', 'Te '+thx+'!CeV'
    options, etest+'_t3', 'ysubtitle', ''
  endif else begin
  ;options,name1,'colors',[cols.blue,cols.green,cols.red]
;    options, name1, labels = ['V!dex!n', 'V!dey!n', 'V!dez!n'], constant = 0.
    name1 = etest+'_t3'
    tdegap, name1, /overwrite, dt = 600.0
    options, name1, labels = ['TEx', 'TEy', 'TEz'], constant = 0.
    ylim, name1, 10, 10000., 1
    options, name1, 'colors', [2, 4, 6]
    options, name1, 'ytitle', 'TE '+thx+'!CeV'
    options, name1, 'ysubtitle', ''
  endelse

; plot quantities (manipulating the plot quantities for the sake of plot aesthetics)
;kluge for labeling the density, added Npot, 2009-10-12, jmm
  get_data, etest+'_density', data = d
  get_data, etest+'_density_npot', data = d1
  Ne_kluge_name = 'Ne_'+etest+'_kluge'
  If(n_elements(d1.x) Eq n_elements(d.x)) Then Begin
    dummy = fltarr(n_elements(d.y), 3)
    dummy[*, 0] = d1.y
    dummy[*, 1] = d.y
    dummy[*, 2] = d.y
    store_data, Ne_kluge_name, data = {x:d.x, y:dummy}
    options, Ne_kluge_name, labels = ['Npot', 'Ni', 'Ne']
    options, Ne_kluge_name, colors = [2, 0, 6]
    options, Ne_kluge_name, 'labflag', 1
  Endif Else Begin
    dummy = fltarr(n_elements(d.y), 2)
    dummy[*, 0] = d.y
    dummy[*, 1] = d.y
    store_data, Ne_kluge_name, data = {x:d.x, y:dummy}
    options, Ne_kluge_name, labels = ['Ni', 'Ne']
    options, Ne_kluge_name, colors = [0, 6]
    options, Ne_kluge_name, 'labflag', 1
  Endelse
  store_data, thx+'_Nie'+mtyp[j], data = [itest+'_density', Ne_kluge_name]
;  options, thx+'_Nie'+mtyp[j], 'ytitle', 'Ni,e '+thx+'!C1/cm!U3'
;  options, thx+'_Nie'+mtyp[j], 'ytitle', 'Ni,e '+thx
  options, thx+'_Nie'+mtyp[j], 'ytitle', 'Ni,e '+thx+'!C1/cc'
  options, thx+'_Nie'+mtyp[j], 'ysubtitle', ''
  nameti=itest+'_t3'
  namete=etest+'_t3'
  store_data, thx+'_Tie'+mtyp[j], data = [nameti,namete]
  options, thx+'_Tie'+mtyp[j], 'ytitle', 'Ti,e '+thx+'!CeV'
  options, thx+'_Tie'+mtyp[j], 'ysubtitle', ''
  options,nameti,'labels',['Ti!9'+string(120B)+'!X','','Ti!9'+string(35B)+'!X']
  options,namete,'labels',['    Te!9'+string(120B)+'!X','    ','    Te!9'+string(35B)+'!X']
  options, thx+'_Tie'+mtyp[j], 'labflag', 1
  options,nameti, 'colors', [2,2, 4]
  options,namete, 'colors', [6,6, 0]
Endfor
SKIP_ESA_LOAD:

; load gmag data
;----------------

load_position='gmag'

thm_load_pseudoAE,datatype='ae'
if tnames('thg_idx_ae') eq '' then begin
  store_data,'thg_idx_ae',data={x:time_double(date)+dindgen(2), y:replicate(!values.d_nan,2)}
endif
options,'thg_idx_ae',ytitle='THEMIS!CAE Index'

SKIP_GMAG_LOAD:

; load ASK data and plot 3 specific ones (can be changed)
;---------------------------------------------------------

load_position='asi'

thm_load_ask, /verbose

SKIP_ASI_LOAD:

asi_sites = tnames('*ask*')

filler = fltarr(2, 10)          ; (10 chosen arbitrarily)
filler[*, *] = float('NaN')
;Harald requests using FSMI as first choice:
fsmi_site = tnames('*ask*fsmi*')
if fsmi_site[0] ne '' then copy_data, fsmi_site[0], 'Keogram' else begin
  if asi_sites[0] ne '' then copy_data, asi_sites[0], 'Keogram' else store_data, 'Keogram', data = {x:time_double(date)+findgen(2), y:filler, v:findgen(10)}
endelse

; Get position info
;---------------------------------------------------------

load_position='mode'

thm_cotrans,thx+'_state_pos',out_suf='_gse',out_coord='gse'
get_data, thx+'_state_pos_gse',data=tmp
store_data,thx+'_state_pos_gse_x',data={x:tmp.x,y:tmp.y[*,0]/6370.}
	options,thx+'_state_pos_gse_x','ytitle',thx+'_X-GSE'
store_data,thx+'_state_pos_gse_y',data={x:tmp.x,y:tmp.y[*,1]/6370.}
	options,thx+'_state_pos_gse_y','ytitle',thx+'_Y-GSE'
store_data,thx+'_state_pos_gse_z',data={x:tmp.x,y:tmp.y[*,2]/6370.}
	options,thx+'_state_pos_gse_z','ytitle',thx+'_Z-GSE'

SKIP_POS_LOAD:

load_position='mode'

; make tplot variable tracking the sample rate (0=SS,1=FS,2=PB,3=WB)
;-------------------------------------------------------------------
sample_rate_var = thm_sample_rate_bar(date, dur, sc, /outline)
SKIP_SURVEY_MODE:
load_position='bound'

; final tplot preparations
;--------------------------


load_position='plot'

; plot it!
thm_spec_lim4overplot, thx+'_peif_en_eflux', zlog = 1, ylog = 1, /overwrite
thm_spec_lim4overplot, thx+'_peef_en_eflux', zlog = 1, ylog = 1, /overwrite
thm_spec_lim4overplot, thx+'_peir_en_eflux', zlog = 1, ylog = 1, /overwrite
thm_spec_lim4overplot, thx+'_peer_en_eflux', zlog = 1, ylog = 1, /overwrite
ssti_name=thx+'_psif_en_eflux'
sste_name=thx+'_psef_en_eflux'
thm_spec_lim4overplot, ssti_name, zlog = 1, ylog = 1, /overwrite
;                       zmin = 1d1, zmax = 5d7
;reset sst ylimit maxima to 3.0e6
get_data, ssti_name, data = d
If(is_struct(d)) Then ylim, ssti_name, min(d.v), 3.0e6, 1
thm_spec_lim4overplot, sste_name, zlog = 1, ylog = 1, /overwrite
;                       zmin = 1d1, zmax = 5d7
get_data, sste_name, data = d
If(is_struct(d)) Then ylim, sste_name, min(d.v), 3.0e6, 1


;thm_spec_lim4overplot will override any z-axis min/max with
;the min/max of the data if any zeros are found, therefore
;the z-range must be set again here for consistant plots
zlim, thx+'_peif_en_eflux', 1d3, 7.5d8, 1
zlim, thx+'_peef_en_eflux', 1d4, 7.5d8, 1
zlim, thx+'_peir_en_eflux', 1d3, 7.5d8, 1
zlim, thx+'_peer_en_eflux', 1d4, 7.5d8, 1
zlim, ssti_name, 1d0, 5d7, 1
zlim, sste_name, 1d0, 5d7, 1


SKIP_BOUNDS:

tplot_options, 'lazy_ytitle', 0 ; prevent auto formatting on ytitle (namely having carrage returns at underscores)


roi_bar = thm_roi_bar(thx+'_state_roi')

!p.background=255.
!p.color=0.
time_stamp,/off
loadct2,43
!p.charsize=0.6

probes_title = ['P5',  'P1',  'P2',  'P3', 'P4']
scv = strcompress(strlowcase(sc[0]),/remove_all)
pindex = where(vsc Eq scv) ;this is always true for one probe by the time we are here
tplot_options,'ygap',0.0D

;For esa data we would like to plot full mode if possible, but reduced
;mode if no full mode is available
esaif_flux_name = thx+'_peif_en_eflux'
If(ok_esai_flux[0] Eq 0) Then Begin  ;esa ion flux is not present full resolution,
  If(ok_esai_flux[1]) Then esaif_flux_name = thx+'_peir_en_eflux'
Endif
esaif_v_name = thx+'_peif_velocity_dsl'
If(ok_esai_moms[0] Eq 0) Then Begin
  If(ok_esai_moms[1]) Then esaif_v_name = thx+'_peir_velocity_dsl'
Endif
esaef_flux_name = thx+'_peef_en_eflux'
If(ok_esae_flux[0] Eq 0) Then Begin  ;esa electron flux is not present full resolution, rename if possible
  If(ok_esae_flux[1]) Then esaef_flux_name = thx+'_peer_en_eflux'
Endif
esaef_v_name = thx+'_peef_velocity_dsl'
If(ok_esae_moms[0] Eq 0) Then Begin
  If(ok_esae_moms[1]) Then esaef_v_name = thx+'_peer_velocity_dsl'
Endif
esaf_t_name = thx+'_Tief'       ;T and N are done for ions, electrons together
esaf_n_name = thx+'_Nief'
If(ok_esai_moms[0] Eq 0) Then Begin
  If(ok_esai_moms[1]) Then Begin
    esaf_t_name = thx+'_Tier'
    esaf_n_name = thx+'_Nier'
  Endif
Endif
    
vars_full = ['thg_idx_ae', roi_bar, 'Keogram', thx+'_fgs_gse', $
             esaf_n_name, esaif_v_name, esaf_t_name, 'sample_rate_'+sc, $
             ssti_name, esaif_flux_name, sste_name,  $
             esaef_flux_name, thx+'_fb_*', thx+'_pos_gse_z']

tplot, vars_full, title = probes_title[pindex[0]]+' (TH-'+strupcase(sc)+')', $
  var_label = [thx+'_state_pos_gse_z', thx+'_state_pos_gse_y', thx+'_state_pos_gse_x']

; make pngs
;-----------
;there are three different types of png plots, the 24hr will include
;full mode, the 6hr and 2hr plots will contain reduced mode, if there
;is reduced data.
if keyword_set(makepng) then begin
  esair_flux_name = thx+'_peir_en_eflux'
  If(ok_esai_flux[1] Eq 0) Then Begin
    If(ok_esai_flux[0]) Then esair_flux_name = thx+'_peif_en_eflux'
  Endif
  esair_v_name = thx+'_peir_velocity_dsl'
  If(ok_esai_moms[1] Eq 0) Then Begin
    If(ok_esai_moms[0]) Then esair_v_name = thx+'_peif_velocity_dsl'
  Endif
  esaer_flux_name = thx+'_peer_en_eflux'
  If(ok_esae_flux[1] Eq 0) Then Begin
    If(ok_esae_flux[0]) Then esaer_flux_name = thx+'_peef_en_eflux'
  Endif
  esaer_v_name = thx+'_peer_velocity_dsl'
  If(ok_esae_moms[1] Eq 0) Then Begin
    If(ok_esae_moms[0]) Then esaer_v_name = thx+'_peef_velocity_dsl'
  Endif
  esar_t_name = thx+'_Tier' ;T and N are done for ions, electrons together
  esar_n_name = thx+'_Nier'
  If(ok_esai_moms[1] Eq 0) Then Begin
    If(ok_esai_moms[0]) Then Begin
      esar_t_name = thx+'_Tief'
      esar_n_name = thx+'_Nief'
    Endif
  Endif
  vars06 = ['thg_idx_ae', roi_bar, 'Keogram', thx+'_fgs_gse', $
             esar_n_name, esair_v_name, esar_t_name, 'sample_rate_'+sc, $
             ssti_name, esair_flux_name, sste_name,  $
             esaer_flux_name, thx+'_fb_*', thx+'_pos_gse_z']
  vars02 = ['thg_idx_ae', roi_bar, 'Keogram', thx+'_fgs_gse', $
             esar_n_name, esair_v_name, esar_t_name, 'sample_rate_'+sc, $
             ssti_name, esair_flux_name, sste_name,  $
             esaer_flux_name, thx+'_fb_*', thx+'_pos_gse_z']
  thm_gen_multipngplot, thx+'_l2_overview', date, directory = directory, $
    vars24 = vars_full, vars06 = vars06, vars02 = vars02
endif ; makepng

SKIP_DAY:
message, /info, 'Returning:'
Return

end
