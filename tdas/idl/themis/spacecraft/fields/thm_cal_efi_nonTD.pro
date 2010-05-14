

;+
;
;THM_UPDATE_EFI_LABELS.PRO
;
;PURPOSE:
;To be used on THEMIS EFI 3-vectors.  Labels x-axis as "E12 (<coordinate system in DLIMITS.LABLES>)" for spinning coordinate systems and
;"Ex (<coord. sys. in DLIMITS.LABELS>)" for despun coordinate systmes.  Y and Z axes are upadated correspondingly.  Call after coordinate
;transformations.
;
;SYNTAX:
;  thm_update_efi_labels ,<String>
;
;Arguments:
;  <String>: A TPLOT variable name referring to a 3-vector TPLOT variable.
;
;Code: W. Michael Feuerstein, 7/2008.
;
;-

pro thm_update_efi_labels ,tvarname

compile_opt idl2, hidden

coord=cotrans_get_coord(tvarname)
if where(coord eq ['spg','ssl']) ge 0 then options,/default,tvarname,'labels',['E12','E34','E56']+' ('+coord+')' else $
  options,/default,tvarname,'labels',['Ex','Ey','Ez']+' ('+coord+')'

end


;+
;Procedure: THM_CAL_EFI
;
;Purpose:  Converts raw EFI (V, EDC, and EAC waveform) data into
;          physical quantities.
;
;keywords:
;   /VERBOSE or VERBOSE=n ; set to enable diagnostic message output.
;		higher values of n produce more and lower-level diagnostic
;		messages.
;   datatype  - Default setting is to calibrate all raw quantites and also
;               produce all _0 and _dot0 quantities that are passed in
;               DATATYPE kw.
;   probe     - Defaults to all probes.
;   valid_names - Return valid datatypes, print them, and return.
;   coord     - Coordinate system of output.
;   TEST: Disables selected /CONTINUE to MESSAGE.  For QA testing only.
;
;keyword parameters for _dot0 computation:
;   max_angle - Maximum angle of B field to spin axis to calculate _dot0.
;               Typical = 80 degrees.  No default.
;   min_bz    - Minimum value of Bz.  Typical value is 2.0 nT.
;               Default= 1.0 nT.  Not compatible with max_angle keyword.
;   max_bxy_bz- Maximum value of abs(bx/bz) or abs(by/bz).  Typical value is
;               5. ~= tan(79 degrees) (think of Bx/Bz).
;               Default is not to use this method (no default value).
;   bz_offset - Offset in nT that will be added to Z axis measurement of B.
;               Defaults to 0.0.
;   fgm_datatype - 'fgl', 'fgh', 'fgs' or 'fge'
;
;Example:
;   thm_cal_efi, probe='c'
;
;Modifications:
;  Added boom_shorting_factor_e12, boom_shorting_factor_e34,
;    offset_e12, offset_e34, offset_dsc_x, offset_dsc_y
;    fields to data_att structure in the default limits structure.
;    Also, added mechanism to fill new field from
;    THM_GET_EFI_CAL_PARS.PRO, W.M.Feuerstein, 2/26-27/2008.
;  Changed "History" to "Modifications" in header, made "1000" a float ("."),
;    switched all internal calibration parameters over to those read through
;    THM_GET_EFI_CAL_PARS.PRO, WMF, 3/12/2008.
;  Updated DATA_ATT fields to match revised cal. files, WMF, 3/12/2008.
;  Updated error handling, updated doc'n, WMF, 3/12/2008.
;  Updated doc'n, made datatypes more consistent (speed), WMF, 3/14/2008.
;  Changed "no default" for MIN_BZ kw to default=1.0 nT, fixed MAX_ANGLE
;    and MIN_BZ kwd's "both set" handling, WMF, 3/14/2008.
;  Changed an NaN multiplication to an assignment (speed), simplified MIN_BZ
;    default assignment, turned on good practice compile options,
;    reorganized opening statements (flow), turned off ARG_PRESENT(MAX_BXY_BZ)
;    bug (no default for this kw), updated doc'n, WMF, 3/17/2008.
;  Now calculates Ez (_dot0 section) AFTER implementing MAX_BXY_BZ kw as per
;    J. Bonnell's request (it also happens to be faster), updated doc'n,
;    WMF, 3/18/2008.
;  Removed redundant datatype conversions, WMF, 3/20/2008.
;  Using BOOM_SHORTING_FACTOR in field calibration, WMF, 3/20/2008 (Th).
;  Fixed potential logical vs. bitwise conditional bug, WMF, 3/21/2008 (F).
;  Updated doc'n, WMF, 3/27/2008.
;  Removed "TWEAK_GAINS" kw to THM_EFI_DESPIN.PRO per J.Bonnell's req.,
;    WMF, 4/4/2008 (F).
;  Added TEST kw to disable certain /CONTINUE to MESSAGE, and passed TEST
;    through to THM_GET_EFI_CAL_PARS.PRO, WMF, 4/7/2008 (M).
;  Implemented time-dependent EAC/EDC gain conditional, WMF, 4/22/2008 (Tu).
;  Made sure input to COORD kw is case-insensitive, WMF, 6/26/2008.
;  Made sure 'E12','E34','E56' labels go with SSL and SPG coordinates, and
;    'Ex','Ey','Ez' go with all other coordinate systems, WMF, 7/8/2008.
;  Fixed "COORD='gse'" crash, wrote THM_UPDATE_EFI_LABELS.PRO to handle labelling, WMF, 7/15/2008.
;  Renamed from "thm_cal_efi.pro" to "thm_cal_efi_nonTD.pro", WMF, 9/9/2008.
;
;Notes:
;	-- fixed, nominal calibration pars used (gains and
;          frequency responses), rather than proper time-dependent parameters.
;
; $LastChangedBy: mfeuerstein $
; $LastChangedDate: 2008-09-09 17:30:11 -0700 (Tue, 09 Sep 2008) $
; $LastChangedRevision: 3471 $
; $URL $
;-

pro thm_cal_efi, probe=probe, datatype=datatype, files=files,trange=trange,$
                 valid_names=valid_names, verbose=verbose, coord=coord, $
                 in_suffix=in_suf,out_suffix=out_suf, max_angle=max_angle, $
                 min_bz=min_bz, max_bxy_bz=max_bxy_bz, bz_offset=bz_offset, $
                 fgm_datatype=fgm_datatype, test=test

compile_opt idl2, strictarrsubs              ;Bring this routine up to date.

if ~keyword_set(test) then test = 0

;Check for "max_angle" and "min_bz" kwd's both being set:
;========================================================
;if keyword_set(max_angle) and keyword_set(min_bz) then begin ;Fails on var=0.
if size(max_angle,/type) ne 0 && size(min_bz,/type) ne 0 then begin ;This way.
   message,/cont, $
     'MIN_BZ and MAX_ANGLE keywords are mutually exclusive.  Returning...'
   return
endif


thm_init


vb = size(verbose, /type) ne 0 ? verbose : !themis.verbose


;Define valid EFI "datatypes":
;=============================
primary_datatypes = [ 'vaf', 'vap', 'vaw', 'vbf', 'vbp', 'vbw', 'eff', $
		      'efp', 'efw'] ; note that efi_dq is deleted.
u0s=['eff_0','efp_0','efw_0']
udot0s=['eff_dot0','efp_dot0','efw_dot0']
efi_valid_names = [primary_datatypes, u0s, udot0s]


;Return valid datatypes (and return), if they are called for:
;============================================================
if arg_present( valid_names) then begin
   valid_names = efi_valid_names
   message, /info, string( strjoin( efi_valid_names, ','), $
			   format='( "Valid names:",X,A,".")')
   return
endif


if not keyword_set(coord) then coord='dsl' else coord=strlowcase(coord)


;Set in_suf/out_suf variables to null, if not set:
;=================================================
if not keyword_set(in_suf) then in_suf=''
if not keyword_set(out_suf) then out_suf=''


;Define "probes" variable ("f" is not a valid probe unless it is the only
;element).  Return if "probes" does not get defined:
;===================================================
vprobes = ['a','b','c','d','e']
if n_elements(probe) eq 1 then if probe eq 'f' then vprobes = ['f']
if not keyword_set(probe) then probes = vprobes $
  else probes = thm_check_valid_name(strlowcase(probe), vprobes, /include_all)
if not keyword_set(probes) then return
if keyword_set(vb) then printdat, probes, /value, varname='Probes'


;Define "dts" (datatypes) variable.  Return if "dts" does not get defined:
;=========================================================================
if not keyword_set(datatype) then dts = efi_valid_names $
  else dts = thm_check_valid_name(strlowcase(datatype), efi_valid_names, $
                                  /include_all)
if not keyword_set(dts) then return
if keyword_set(vb) then printdat, dts, /value, varname='Datatypes'


;Make min_bz = 1.0 nT the default (as long as MAX_ANGLE is not defined!):
;========================================================================
if ~size(min_bz,/type) && ~size(max_angle,/type) then min_bz = 1.0 ;nT


;BZ_OFFSET default:
;==================
if ~size(bz_offset,/type) then bz_offset=0.0


;Loop on "probes":
;==================
for s=0L,n_elements(probes)-1L do begin
   sc = probes[s]

   ;Loop on "dts":
   ;==============
   for n=0L, n_elements(dts)-1L do begin
      name = dts[ n]


      ;NAMERAW= 'aaa', NAME= (e.g.) 'aaa_dot0':
      ;========================================
      if where(name eq u0s or name eq udot0s) ne -1 then begin
         nameraw=strmid(name,0,3)
      endif else nameraw=name

      ;Define tplot var. names and get raw data:
      ;=========================================
      tplot_var_raw = thm_tplot_var( sc, nameraw)+in_suf
      tplot_var = thm_tplot_var( sc, name)+out_suf
      get_data, tplot_var_raw, data=d, limit=l, dlim=dl
      if keyword_set( vb) then $
        message, /cont, string( tplot_var, format='("working on TPLOT variable",X,A)')


      ;Check to see if the data has already been calibrated:
      ;=====================================================
      if( thm_data_calibrated( dl)) then begin
         if nameraw eq name then begin
            if keyword_set( vb) then begin
               message, /cont, string( tplot_var, format= $
                                       '(A,X,"has already been calibrated.")')
            endif

         ;Else it's gotta be either "_0", or "_dot0", so get cal_pars ("cp") and
         ;transform to 'dsl' (via THM_COTRANS for non-'dsl' and non-'spg', else via THM_EFI_DESPIN.PRO):
         ;==============================================================================================
         endif else begin
            thm_get_efi_cal_pars, d.x, nameraw, probes, cal_pars=cp,test=test
            if where(cotrans_get_coord(dl) eq ['dsl','spg']) lt 0 then begin
              thm_cotrans, tplot_var_raw,tplot_var,out_coord='dsl'
	    endif else begin
              thm_efi_despin, sc, nameraw, cp.dsc_offset, [1,1,1], $          ;Pass [1,1,1] in order to not duplicate boom_shorting factor.
	      tplot_name=tplot_var_raw, $
	      newname=tplot_var
            endelse
         endelse


      ;Otherwise, proceed with the RAW->PHYS transformation:
      ;=====================================================
      endif else begin
         hed_tplot_var=thm_tplot_var(sc,nameraw)
         tplot_var_hed = hed_tplot_var + '_hed'
         get_data, tplot_var_hed, data=d_hed, limit=l_hed, dlim=dl_hed


         ;Check that returned data and hed structures are structures
         ;(get_data returns 0 if no TPLOT variable exists):
	 ;=================================================
         if (size( d, /type) eq 8) and (size( d_hed, /type) eq 8) then begin

            hed_data = thm_unpack_hed( name, d_hed.y)


	    ;Match datatype, get corresponding calibration parameters,
	    ;apply calibration, and update default limits structure:
	    ;=======================================================
            switch strlowcase( name) of
               'vaf':
               'vbf':
               'vap':
               'vbp':
               'vaw':
               'vbw':	begin
                  tbeg = min( d.x, max=tend)
                  thm_get_efi_cal_pars, d.x, name, probes, $
                    cal_pars=cp, test=test
                  ;thm_get_efi_cal_pars, tbeg, tend, name, probes, $
                  ;  cal_pars=cp, test=test
                  res = fltarr( size( d.y, /dimensions))
                  for icomp=0L,5L do begin
                     res[*, icomp] = $
                       cp.gain[icomp]*(d.y[*, icomp] - cp.offset[icomp])
                  endfor

                  ;; update the DLIMIT elements to reflect RAW->PHYS
                  ;; transformation, coordinate system, etc.

                  units = cp.units        ;Now taken from cal. file.

                  str_element, dl, 'data_att', data_att, success=has_data_att
                  if has_data_att then begin
                     str_element, data_att, 'data_type', 'calibrated', /add
                  endif else data_att = { data_type: 'calibrated' }
                  str_element, data_att, 'coord_sys',  'efi_sensor', /add
                  str_element, data_att, 'units', units, /add
                  str_element, data_att, 'cal_par_time', cp.cal_par_time, /add

                  ;"Documented calibration factors" to match expanded
                  ;cal. file params. (3/12/2008):
                  ;==============================
                  str_element,data_att,'offset',cp.offset,/add
                  str_element,data_att,'gain',cp.gain,/add
                  str_element,data_att,'boom_length',cp.boom_length,/add
                  str_element,data_att,'boom_shorting_factor', $
                    cp.boom_shorting_factor,/add

                  str_element, dl, 'data_att', data_att, /add

                  str_element, dl, 'ytitle', string(tplot_var, units, format= $
                                                    '(A,"!C!C[",A,"]")'), /add

                  str_element, dl, 'units', units, /add
                  str_element, dl, 'labels', $
                               [ 'V1', 'V2', 'V3', 'V4', 'V5', 'V6'], /add
                  str_element, dl, 'labflag', 1, /add
                  str_element, dl, 'colors', [ 1, 2, 3, 4, 5, 6]

                  ;; store the transformed spectra back into the original
                  ;; TPLOT variable.
                  store_data, tplot_var, $
                              data = { x:d.x, y:res, v:d.v }, $
                              lim=l, dlim=dl

                  break
               end  ; end of { VAF, VBF, VAP, VBP, VAW, VBW} calibration clause.
               'eff':
               'efp':
               'efw':
               'efp_0':
               'eff_0':
               'efw_0':
               'efp_dot0':
               'efw_dot0':
               'eff_dot0': begin ; { EFF, EFP, EFW} calibration clause.

                  tbeg = min( d.x, max=tend)
                  thm_get_efi_cal_pars, d.x, nameraw, probes, $
                    cal_pars=cp, test=test
                  ;thm_get_efi_cal_pars, tbeg, tend, nameraw, probes, $
                  ;  cal_pars=cp, test=test

                  ;These are boom lengths in meters:
		  ;=================================
		  ;e12=49.6           ;These are now taken from the cal. file.
                  ;e34=40.4
                  ;e56=5.63
                  exx=cp.boom_length*cp.boom_shorting_factor

                  res = fltarr( size( d.y, /dimensions))
                  ;
                  ;
                  ;==================
                  ;Calculate E field:
                  ;==================
                  ;
                  ;GAIN is time-independent case:
                  ;==============================
                  if size(cp.gain,/n_dimensions) le 1 then begin
		    for icomp=0L,2L do begin
		      res[ *, icomp] = $
			-1000.*cp.gain[icomp] * $
			(d.y[*,icomp] - cp.offset[icomp])/exx[icomp]
		    endfor
                  ;
                  ;GAIN is an array case:
                  ;======================
                  endif else begin
		    for icomp=0L,2L do begin
		      res[ *, icomp] = $
			-1000.*cp.gain[*,icomp] * $
			(d.y[*,icomp] - cp.offset[icomp])/exx[icomp]
		    endfor
                  endelse
                  ;
                  ;; update the DLIMIT elements to reflect RAW->PHYS
                  ;; transformation, coordinate system, etc.

                  ;cp.units='mV/m'    ;This is now taken from the cal. file.
                  units = cp.units

                  str_element, dl, 'data_att', data_att, success=has_data_att
                  if has_data_att then begin
                     str_element, data_att, 'data_type', 'calibrated', /add
                  endif else data_att = { data_type: 'calibrated' }
                  str_element, data_att, 'coord_sys',  'spg', /add
                  str_element, data_att, 'units', units, /add
                  str_element, data_att, 'cal_par_time', cp.cal_par_time, /add


                  ;"Documented calibration factors" to match expanded
                  ;cal. file params. (3/12/2008):
                  ;==============================
                  str_element,data_att,'offset',cp.offset,/add
                  str_element,data_att,'gain',cp.gain,/add
                  str_element,data_att,'boom_length',cp.boom_length,/add
                  str_element,data_att,'boom_shorting_factor', $
                    cp.boom_shorting_factor,/add
                  str_element,data_att,'dsc_offset',cp.dsc_offset,/add


                  str_element, dl, 'data_att', data_att, /add

                  str_element, dl, 'ytitle', string( tplot_var, units, format= $
                                                     '(A,"!C!C[",A,"]")'), /add

                  str_element, dl, 'labels', [ 'E12', 'E34', 'E56'], /add
                  str_element, dl, 'labflag', 1, /add
                  str_element, dl, 'colors', [ 2, 4,6]


                  ;Store the transformed spectra into the output
                  ;TPLOT variable:
	           	  ;===============
                  store_data, tplot_var, $
                              data = { x:d.x, y:res, v:d.v }, $
                              lim=l, dlim=dl


	          ;If these are dot0 or 0 datatypes, then transform to dsl
	          ;for further processing, else transform according to coord
	          ;kw (if not 'spg'), but transform to dsl first!
                  ;(labels: spun coord. systems get "E12,34,56"; despun systems get "Ex,y,z"):
                  ;===========================================================================
                  if where(name eq [udot0s, u0s]) ge 0 then begin
                    thm_efi_despin, sc, nameraw, cp.dsc_offset, [1,1,1], $        ;Pass [1,1,1] in order to not duplicate boom_shorting factor.
                      tplot_name=tplot_var, $
                      newname=tplot_var
                    thm_update_efi_labels,tplot_var
                  endif else if keyword_set(coord) && coord ne 'spg' then begin
		    thm_efi_despin, sc, nameraw, cp.dsc_offset, [1,1,1], $        ;Pass [1,1,1] in order to not duplicate boom_shorting factor.
		      tplot_name=tplot_var, $
		      newname=tplot_var
                    if coord ne 'dsl' then thm_cotrans,tplot_var,out_coord=coord
                  endif
                  thm_update_efi_labels,tplot_var
                  break
               end              ; of { EFF, EFP, EFW} calibration clause.
               else:	begin   ; improperly defined name of EFI quantity.
                  message,/continue, $
                    string( tplot_var_raw, tplot_var_hed, format= $
                    '("Improperly defined name of EFI quantity ("'+ $
                    ',A,X,A,").")')

               end

            endswitch

         endif else begin	; necessary TPLOT variables not present.
            if keyword_set(vb) then $
               message, /continue, $
                        string( tplot_var_raw, tplot_var_hed, format= $
                                '("necessary TPLOT variables (",A,X,A,") ' + $
                                'not present for RAW->PHYS transformation.")')

         endelse
      endelse                   ; done with RAW->PHYS transformation
                                ;stop


      ;Finish processing of _0 quantity, if present:
      ;=============================================
      if where(name eq u0s) ne -1 then begin
         get_data, tplot_var, data=d, limit=l, dlim=dl
         if size(d, /type) ne 8 then continue
         res = d.y
         res[*,2]=0.0
         ;
         store_data, tplot_var, $
           data = { x:d.x, y:res, v:d.v }, $
           lim=l, dlim=dl

         if keyword_set(coord) && coord ne 'dsl' then begin
            if coord ne 'spg' and coord ne 'ssl' then begin
               thm_update_efi_labels,tplot_var
               message, /continue, $
                        'Warning: coord keyword ignored: ' + $
                        'only ssl, dsl, or spg are physically meaningful ' + $
                        'coordinate systems for _0 quantites.'
            endif else begin
               thm_cotrans,tplot_var,out_coord=coord
               thm_update_efi_labels,tplot_var
            endelse
         endif
      endif


      ;===============================================
      ;Finish processing of _dot0 quantity, if present:
      ;===============================================
      if where(name eq udot0s) ne -1 then begin
         get_data, tplot_var, data=d, limit=l, dlim=dl
         if size(d, /type) ne 8 then continue
         res = d.y

         ;==========================================
         ;Temporarily load FGM data as a TPLOT var.:
         ;==========================================
         suff = '_thm_cal_efi_priv'
         thm_load_fgm,probe=sc,level='l2',coord='dsl', suffix = suff
         if keyword_set(fgm_datatype) then ftype = fgm_datatype $
         else begin
            if nameraw eq 'efp' || nameraw eq 'efw' $
            then ftype='fgh' $
            else ftype='fgl'
         endelse

         if vb ge 2 then print, 'Using '+ftype

         ;===========
         ;Degap data:
         ;===========
         tdegap, 'th'+sc+'_'+ftype+'_dsl'+suff, dt = cp.cal_par_time, $
                 margin = cp.cal_par_time * 0.2, /overwrite

         ;=======================================================
         ;Get relevant data and delete temporary TPLOT variables:
         ;=======================================================
         get_data,'th'+sc+'_'+ftype+'_dsl'+suff, $
                  data=thx_fgx_dsl, limit=fl, dlim=fdl
         del_data, '*'+suff


         ;==================================
         ;Interpolate FGM data to EFI times:
         ;==================================
         Bx = interpol(thx_fgx_dsl.y[*,0],thx_fgx_dsl.x, d.x)
         By = interpol(thx_fgx_dsl.y[*,1],thx_fgx_dsl.x, d.x)
         Bz = interpol(thx_fgx_dsl.y[*,2],thx_fgx_dsl.x, d.x) + bz_offset


         ;=============
         ;Set z to NaN:
         ;=============
         res[*,2] = !values.f_nan   ;Assign.


         ;==================================================
         ;Apply max_angle kw, if present.  If not, apply
         ;min_bz kw, if present.  If not, let all B's be good.
         ;If there are no good elements, then continue loop:
         ;==================================================
         if keyword_set(max_angle) then begin
            angle=acos(Bz/sqrt(Bx^2+By^2+Bz^2))*180d/!dpi
            good = where(abs(angle) le max_angle, ngood)
            if ngood eq 0 then continue
         endif else if keyword_set(min_bz) then begin
            good = where(abs(Bz) ge min_bz, ngood)
            if ngood eq 0 then continue
         endif else good = lindgen(n_elements(d.x))


         ;Calculate Bx over Bz, By over Bz:
         ;=================================
         bx_bz = Bx[good]/Bz[good]
         by_bz = By[good]/Bz[good]


         ;===================================
         ;Apply max_bxy_bz kw, if present.  If not, keep all.
         ;If no good elements, continue loop:
         ;===================================
         if keyword_set(max_bxy_bz) then begin
           keep = where(abs(bx_bz) lt max_bxy_bz and abs(by_bz) lt max_bxy_bz,$
                        nkeep)
           if nkeep eq 0 then continue
         endif else keep = lindgen(ngood)
         goodkeep=good[keep]              ;Otherwise this gets eval'td 3X.


         ;Calculate Ez and assign directly to result:
         ;===========================================
          res[goodkeep,2] = -(bx_bz[keep]*res[goodkeep,0] + $
                              by_bz[keep]*res[goodkeep,1])


         ;Store result:
         ;=============
         store_data, tplot_var, $
                     data = { x:d.x, y:res, v:d.v }, $
                     lim=l, dlim=dl

	 ;At this point, TPLOT_VAR is in dsl.  If COORD ne 'dsl', then transform via THM_COTRANS.PRO
         ;and set lables (spun coord. systems get "E12,34,56"; despun systems get "Ex,y,z"):
         ;==================================================================================
         if keyword_set(coord) && coord ne 'dsl' then begin
            thm_cotrans,tplot_var,out_coord=coord
            thm_update_efi_labels, tplot_var
         endif

      endif

   endfor                       ; loop over names.
endfor                          ; loop over spacecraft.

end
