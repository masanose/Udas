;+
;Procedure: THM_CAL_FIT
;
;Purpose:  Converts raw FIT parameter data into physical quantities.
;keywords:
;   /VERBOSE or VERBOSE=n ; set to enable diagnostic message output.
;		higher values of n produce more and lower-level diagnostic messages.
;   /ALL
;   /no_cal	will not apply boom shortening factor or Ex offset defaults
;
;Example:
;   thm_cal_fit, /all
;
;Modifications:
;  Corrected (reversed) polarity of Zscale in CPAR, WMFeuerstein, 5/13/2008.
;  Call THM_GET_EFI_CAL_PARS.PRO and calibrate 'efs' data with EFI parameters, WMF, 5/13/2008.
;  Mods per McFadden and Vassilis: Do not subtract spin-independent offset for 'efs'
;    data, NO_CAL kw effectively sets boom_shorting_factor to 1 and spin-dependent offsets
;    to 0, WMF, 6/27/2008.
;
;Notes:
;	-- FGM range changes are not handled properly; RANGE=0 is assumed.
;	-- fixed, nominal calibration pars used, rather than proper time-dependent parameters.
;   -- time-dependent spinn axis offset implemented Hannes 05/25/2007
;   -- fixed trouble reading cal files with extra lines at the end,
;      jmm, 8-nov-2007
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-02-22 13:25:43 -0800 (Mon, 22 Feb 2010) $
; $LastChangedRevision: 7366 $
; $URL $
;-
pro thm_cal_fit, probe=probe, datatype=datatype, files=files,trange=trange, coord=coord,valid_names=valid_names,verbose=verbose,in_suf=in_suf,out_suf=out_suf,no_cal=no_cal


  thm_init
  if not keyword_set(datatype) then datatype=['fgs','efs','fgs_sigma','efs_sigma']
  if n_params() eq 0 then begin
     vprobes = ['a','b','c','d','e']
     vdatatypes = ['fgs', 'efs', 'fit_efit', 'fit_bfit', 'fgs_sigma', 'efs_sigma', 'fit', 'efs_0', 'efs_dot0']
     if keyword_set(valid_names) then begin
        probe = vprobes
        datatypes = vdatatypes
        return
     endif
     if n_elements(probe) eq 1 then if probe eq 'f' then vprobes = ['f']
     if not keyword_set(probe) then probes = vprobes $
     else probes = thm_check_valid_name(strlowcase(probe), vprobes, /include_all)
     if not keyword_set(probes) then return

     dts = 'fit'
     dt_output = thm_check_valid_name(strlowcase(datatype), vdatatypes, $
                                      /include_all)

     if not keyword_set(in_suf) then in_suf = ''
     if not keyword_set(out_suf) then out_suf = ''

     for s=0L,n_elements(probes)-1L do begin
        sc = probes[s]
        case 1 of          ; vassilis 4/28 establish probe number in cal tables
           sc eq 'a' : scn=0 ; vassilis 4/28 establish probe number in cal tables
           sc eq 'b' : scn=1 ; vassilis 4/28 establish probe number in cal tables
           sc eq 'c' : scn=2 ; vassilis 4/28 establish probe number in cal tables
           sc eq 'd' : scn=3 ; vassilis 4/28 establish probe number in cal tables
           sc eq 'e' : scn=4 ; vassilis 4/28 establish probe number in cal tables
        endcase            ; vassilis 4/28 establish probe number in cal tables


                                ;start Hannes 05/25/2007
                                ;get the calfile
        thx = 'th' + sc
        cal_relpathname = thx+'/l1/fgm/0000/'+thx+'_fgmcal.txt'
        cal_file = file_retrieve(cal_relpathname, _extra=!themis)


                                ;read the calibration file
        PRINT,'read calibration file:'
        PRINT,cal_file

        ncal=file_lines(cal_file)
        calstr=strarr(ncal)
        openr,2,cal_file
        readf,2,calstr
        close,2
        ok_cal = where(calstr Ne '', ncal) ;jmm, 8-nov-2007, cal files have carriage returns at the end
        calstr = calstr[ok_cal]

	;define variables
        spinperi=dblarr(ncal)
        offi=dblarr(ncal,3)
        cali=dblarr(ncal,9)
        offi2=dblarr(ncal,3)
        spinperii=dblarr(1)
        offii=dblarr(3)
        calii=dblarr(9)
        utci='2006-01-01T00:00:00.000Z'
        utc=dblarr(ncal)
        utcStr=strarr(ncal)

        for i=0,ncal-1 DO BEGIN
           calstri=calstr(i)
           utci=strmid(calstr(i),0,25)
           reads,strmid(calstr(i),26),offii,calii,spinperii ;
           offi(i,*)=offii
           cali(i,*)=calii
           spinperi(i)=spinperii
           utcStr(i)=utci
                                ;translate time information
           STRPUT, utci, '/', 10
           utc(i)=time_double(utci)
        ENDFOR
        PRINT,'done reading calibration file'
                                ;end Hannes 05/25/2007





        for n=0L, n_elements( dts)-1L do begin
           name = dts[ n]
                                ; this call to string should be replaced with a call to the function used to generate the TPLOT variable name in the first place.
           tplot_var = thm_tplot_var( sc, name)
           if keyword_set( verbose) then $
              message, /info, string( tplot_var, format='("working on TPLOT variable",X,A)')

           get_data, tplot_var+in_suf, data=d, limit=l, dlim=dl
           get_data, tplot_var+'_hed'+in_suf, data=d_hed, limit=l_hed, dlim=dl_hed
                                ; get_data, tplot_var+'_code', data=d_code, limit=l_code, dlim=dl_code
                                ; stop
             store_data, delete = tplot_var+'_hed'+in_suf ;moved into block, to avoid error messages in overplot process, jmm, 30-jun-2009
             store_data, tplot_var+'_hed', data = d_hed, limit = l_hed, dlim = dl_hed

                                ; check that returned data and hed structures are structures (get_data returns 0 if no TPLOT variable exists).
           if (size( d, /type) eq 8) and (size( d_hed, /type) eq 8) then begin


                                ;check that data has not already been calibrated
              if thm_data_calibrated(tplot_var+in_suf) then begin
                 message, tplot_var+in_suf, " has already been calibrated."
              endif
                                ;search fgm calibration for selected time interval ...
            ; start Hannes 05/25/2007
              count=n_elements(d.X)
              Bzoffset=dblarr(count)

              PRINT,'search fgm calibration for selected time interval ...'
              compTime=utc(0)
              refTime=d.X(0)
              i=0
              WHILE ((compTime lt reftime) && (i lt ncal-1)) DO BEGIN
                 i=i+1
                 compTime=utc(i)
                 IF (compTime gt reftime) THEN BEGIN
                    i=i-1
                    BREAK
                 ENDIF
              ENDWHILE
              istart=i
              compTime=utc(i)
              refTime=d.X(count-1L)
                                ;i=0
              WHILE ((compTime lt reftime) && (i lt ncal-1)) DO BEGIN
                 i=i+1
                 compTime=utc(i)
                 IF (compTime gt reftime) THEN BEGIN
                    i=i-1
                    BREAK
                 ENDIF
              ENDWHILE
              istop=i
              PRINT, 'Select calibrations from:'
              FOR i=istart,istop DO BEGIN
                 PRINT, utcStr(i)
              ENDFOR
                                ;end search fgm calibration for selected time interval ...'

			;build the calibrations vector (offset only at this point)
                                ;we still need to implement:
                                ;Gz,Gxy,phi1
              flipxz=[[0,0,-1.],[0,-1.,0],[-1.,0,0]]

              IF (istart eq istop) THEN BEGIN
                 offi2=invert(transpose([cali[istart,0:2],cali[istart,3:5],cali[istart,6:8]]) ## flipxz)##offi(istart,*)
                 Bzoffset(0L:count-1L)=offi2(2) ;
              ENDIF ELSE BEGIN
                 FOR ii=istart,istop DO BEGIN
                    IF (ii eq istart) THEN BEGIN
                       indcal=WHERE(d.X lt utc(ii+1))
                       if (indcal(0) gt -1) THEN BEGIN
                          offi2=invert(transpose([cali[ii,0:2],cali[ii,3:5],cali[ii,6:8]]) ## flipxz)##offi(ii,*)
                          Bzoffset(indcal)=offi2(2)
                       ENDIF
                    ENDIF ELSE BEGIN
                       IF (ii eq istop) THEN BEGIN
                          indcal=WHERE(d.X ge utc(ii))
                          if (indcal(0) gt -1) THEN BEGIN
                             offi2=invert(transpose([cali[ii,0:2],cali[ii,3:5],cali[ii,6:8]]) ## flipxz)##offi(ii,*)
                             Bzoffset(indcal)=offi2(2)
                          ENDIF
                       ENDIF ELSE BEGIN
                          indcal=WHERE((d.X ge utc(ii)) AND (d.X lt utc(ii+1)))
                          if (indcal(0) gt -1) THEN BEGIN
                             offi2=invert(transpose([cali[ii,0:2],cali[ii,3:5],cali[ii,6:8]]) ## flipxz)##offi(ii,*)
                             Bzoffset(indcal)=offi2(2)
                          ENDIF
                       ENDELSE
                    ENDELSE
                 ENDFOR
              ENDELSE
                                ;Bzoffset is an array (for vectoriced processing)
                                ;end Hannes 05/25/2007

              hed_data = thm_unpack_hed( 'fit', d_hed.y)


              adc2nT=50000./2.^24                   ; vassilis 2007-04-03
              rotBxy_angles=[29.95,29.95,29.95,29.95,29.95] ; vassilis 6/2/2007: deg to rotate FIT on spin plane to match DSL on 5/4
              rotBxy=rotBxy_angles(scn) ;  vassilis 4/28: probably should be part of CAL table as well...
              cs=cos(rotBxy*!PI/180.)   ;  vassilis
              sn=sin(rotBxy*!PI/180.)   ;  vassilis

                                ;Bz_offset_table = [4.93,6.14,5.03,8.02,2.99] ; vassilis 4/28
                                ; vassilis 4/28: note the above must be read from 3rd (Z) offset in FGM cal files
                                ;Bzoffset = Bz_offset_table(scn) ;  vassilis 4/28
              str_element, dl, 'data_att', data_att, success=has_data_att
              if has_data_att then begin
                 str_element, data_att, 'data_type', 'calibrated', /add
              endif else data_att = { data_type: 'calibrated' }
              str_element, data_att, 'coord_sys',  'dsl', /add

              lv12=49.6         ;m
              lv34=40.4         ;m
              lv56=5.6          ;m

              cpar = { $
                     e:{ cal_par_time:'2002-01-01/00:00:00', Ascale:-15000.0/(lv12*2.^15), Bscale:-15000.0/(lv12*2.^15), Cscale:-15000.0/(lv12*2.^15), theta:0.0, sigscale:15000./(lv12*2.^15), Zscale:-15000./(lv56*2.^15), units:'mV/m'}, $
                     b:{ cal_par_time:'2002-01-01/00:00:00', Ascale:1.e0, Bscale:1.e0, Cscale:1.e0, theta:0.0, sigscale:1.e0, Zscale:1.e0, units:'nT'} $
                     }          ; vassilis 2007-04-03, changed b scales

                                ; E-field fit (EFI).
              idx = 0L
              dqd = 'efit'
              units = cpar.e.units
              tplot_var_efit = string( tplot_var, dqd, format='(A,"_",A)')
              str_element, dl, 'ytitle', tplot_var_efit, /add
              str_element, dl, 'ysubtitle', '['+units+']',/add
              str_element, dl, 'labels', [ 'A', 'B', 'C', 'Sig', '<Ez>'], /add
              str_element, dl, 'labflag', 1, /add
              str_element, dl, 'colors', [ 1, 2, 3, 4, 5], /add
              str_element, data_att, 'cal_par_time', cpar.e.cal_par_time, /add
              str_element, data_att, 'units', units, /add
              str_element, dl, 'data_att', data_att, /add

              ;********************************************
              ;Save 'efs' datatype before "hard wired" calibrations.
              ;An EFI-style calibration is performed below.
              ;********************************************
              efs = reform(d.y[*,[1,2,4],idx])

              d.y[ *, 0, idx] = cpar.e.Ascale*d.y[ *, 0, idx]
              d.y[ *, 1, idx] = cpar.e.Bscale*d.y[ *, 1, idx]
              d.y[ *, 2, idx] = cpar.e.Cscale*d.y[ *, 2, idx]
              d.y[ *, 3, idx] = cpar.e.sigscale*d.y[ *, 3, idx]
              d.y[ *, 4, idx] = cpar.e.Zscale*d.y[ *, 4, idx]

                                ; store the spin fit parameters.
              if (where(dt_output eq 'fit_efit') ne -1)  then begin
                 store_data, tplot_var_efit, $
                             data = { x:d.x, y:reform( d.y[ *, *, idx])}, $
                             lim=l, dlim=dl
              endif


              dqd = 'efs'+out_suf
              tplot_var_efs = string( strmid(tplot_var,0,3), dqd, format='(A,"_",A)')
              str_element, dl, 'ytitle', tplot_var_efs, /add
              str_element, dl, 'ysubtitle', '['+units+']', /add
              str_element, dl, 'labels', [ 'Ex', 'Ey', 'Ez'], /add
              str_element, dl, 'colors', [ 2, 4, 6], /add
                                ; store the spin fit E-field vector (only the B, C, and <Ez> parameters).

              ;****************************************************************************
              ;This if... endif block contains the EFI-style calibration on 'efs' datatype:
              ;****************************************************************************
              if (where(dt_output eq 'efs') ne -1) $
                or (where(dt_output eq 'efs_0') ne -1) $
                or (where(dt_output eq 'efs_dot0') ne -1) then begin ;todo: EFS start
                ;==================================================================================
                ;Calibrate efs data by applying E12 calibration factors, not despinning,
                ;then applying despun (spin-dependent) calibration factors from E12 (the
                ;spin-independent offset is subtracted on-board):
                ;==================================================================================
                thm_get_efi_cal_pars,d.x,'efs',sc,cal_pars=cp
                if keyword_set(no_cal) then exx=cp.boom_length else exx=cp.boom_length*cp.boom_shorting_factor
                ;
                ;
                ;==================
                ;Calibrate E field:
                ;==================
                ;
                ;Calibrate Ex and Ey spinfits that are derived from E12 only!:
                ; JWB, 18 Nov 2008 -- corrected bug in offset subtraction.
                ;*************************************************************
                for icomp=0,1 do begin
                  efs[ *, icomp] = -1000.*cp.gain[0] * efs[*,icomp]/exx[0]
;                  if not keyword_set(no_cal) then efs[*,icomp] -= cp.dsc_offset[0]
                  if not keyword_set(no_cal) then efs[*,icomp] -= cp.dsc_offset[icomp]

                endfor
                ;
                ;Calibrate Ez spinfit by itself:
                ;*******************************
		efs[ *, 2] = -1000.*cp.gain[2] * efs[*,2]/exx[2]
		if not keyword_set(no_cal) then efs[*,2] -= cp.dsc_offset[2]

                if (where(dt_output eq 'efs') ne -1) then begin
                  store_data, tplot_var_efs, $
                            data = { x:d.x, y:efs}, $
                            lim=l, dlim=dl
                endif

                if keyword_set(coord) && strlowcase(coord) ne 'dsl' then begin
                  thm_cotrans, tplot_var_efs, out_coord = coord, use_spinaxis_correction = 1, use_spinphase_correction = 1
                ;   options, tplot_var_efs, 'ytitle', /def, $
                      ;      string( tplot_var_efs, units, format='(A,"!C!C[",A,"]")'), /add
                endif
              endif ; todo: END efs 
              if (where(dt_output eq 'efs_sigma') ne -1) then begin
                 str_element,dl_sigma,'ytitle',tplot_var_efs+'_sigma',/add
                 str_element,dl_sigma,'ysubtitle','['+units+']',/add
                 str_element,data_att_sigma,'units',units,/add
                 str_element,dl_sigma,'data_att',data_att_sigma,/add
                 store_data,tplot_var_efs+'_sigma',data={x:d.x,y:d.y[*,3,idx]},dl=dl_sigma
              endif

                                ; B-field fit (FGM).
              idx = 1L
              dqd = 'bfit'
              units = cpar.b.units
              tplot_var_bfit = string( tplot_var, dqd, format='(A,"_",A)')
              str_element, dl, 'ytitle', tplot_var_bfit, /add
              str_element, dl, 'ysubtitle','['+units+']', /add
              str_element, dl, 'labels', [ 'A', 'B', 'C', 'Sig', '<Ez>'], /add
              str_element, dl, 'labflag', 1, /add
              str_element, dl, 'colors', [ 1, 2, 3, 4, 5], /add
              str_element, data_att, 'cal_par_time', cpar.b.cal_par_time, /add
              str_element, data_att, 'units', units, /add
              str_element, dl, 'data_att', data_att, /add
			; punt on handling FGM range changes for the moment.
              b_range = 0.
              b_range_fac = 1./2.^b_range
              str_element, dl, 'b_range', b_range, /add
              str_element, dl, 'b_range_fac', b_range_fac, /add

              d.y[ *, 0, idx] = b_range_fac*cpar.b.Ascale*d.y[ *, 0, idx]*adc2nT ;  vassilis
              d.y[ *, 1, idx] = b_range_fac*cpar.b.Bscale*d.y[ *, 1, idx]*adc2nT ;  vassilis
              d.y[ *, 2, idx] = b_range_fac*cpar.b.Cscale*d.y[ *, 2, idx]*adc2nT ;  vassilis
              d.y[ *, 3, idx] = b_range_fac*cpar.b.sigscale*d.y[ *, 3, idx]*adc2nT ;  vassilis
              d.y[ *, 4, idx] = b_range_fac*cpar.b.Zscale*d.y[ *, 4, idx]*adc2nT ;  vassilis
              if (where(dt_output eq 'fit_bfit') ne -1) then begin
                 store_data, tplot_var_bfit, $
                             data = { x:d.x, y:reform( d.y[ *, *, idx])}, $
                             lim=l, dlim=dl
              endif
              dqd = 'fgs'+out_suf
              tplot_var_fgs = string( strmid(tplot_var,0,3), dqd, format='(A,"_",A)')
              str_element, dl, 'ytitle', tplot_var_fgs, /add
              str_element, dl, 'ysubtitle', '['+units+']', /add
              str_element, data_att, 'cal_par_time', cpar.b.cal_par_time, /add
              str_element, data_att, 'units', units, /add
              str_element, dl, 'data_att', data_att, /add
              str_element, dl, 'labels', [ 'Bx', 'By', 'Bz'], /add
              str_element, dl, 'colors', [ 2, 4, 6], /add
              Bxprime= cs*d.y[ *,1,idx]+sn*d.y[ *,2,idx]
              Byprime=-sn*d.y[ *,1,idx]+cs*d.y[ *,2,idx]
              Bzprime=-d.y[ *,4,idx]-Bzoffset ; vassilis 4/28 (SUBTRACTING offset from spinaxis POSITIVE direction)
              dprime=d
              dprime.y[ *,1,idx]=Bxprime ; vassilis DSL
              dprime.y[ *,2,idx]=Byprime ; vassilis DSL
              dprime.y[ *,4,idx]=Bzprime ; vassilis DSL
              fgs=reform(dprime.y[*,[1,2,4],idx])


              if (where(dt_output eq 'fgs') ne -1) then begin
                 store_data, tplot_var_fgs, $
                             data = { x:d.x, y:fgs}, $
                             lim=l, dlim=dl
                 if keyword_set(coord) && strlowcase(coord) ne 'dsl' then begin
                   thm_cotrans, tplot_var_fgs, out_coord = coord, use_spinaxis_correction = 1, use_spinphase_correction = 1
                   options, tplot_var_fgs, 'ytitle', /def, $
                             string( tplot_var_fgs, units, format='(A,"!C!C[",A,"]")'), /add
                 endif
              endif

              if (where(dt_output eq 'fgs_sigma') ne -1) then begin
                 str_element,dl_sigma,'ytitle',tplot_var_fgs+'_sigma',/add
                 str_element,dl_sigma,'ysubtitle','['+units+']',/add
                 str_element,data_att_sigma,'units',units,/add
                 str_element,dl_sigma,'data_att',data_att_sigma,/add

                 store_data,tplot_var_fgs+'_sigma',data={x:d.x,y:d.y[*,3,idx]},dl=dl_sigma
              endif


              thx_efs_0=efs
              thx_efs_0[*,2]=0

              str_element, dl, 'labels', [ 'Ex', 'Ey', 'Ez'], /add
              str_element, dl, 'colors', [ 2, 4, 6], /add
              str_element, data_att, 'cal_par_time', cpar.e.cal_par_time, /add
              str_element, data_att, 'units', 'mV/m',/add
              str_element, dl, 'data_att', data_att, /add
              str_element, dl, 'ytitle',thx+'_efs_0', /add
              str_element, dl, 'ysubtitle','['+units+']', /add
              if where(dt_output eq 'efs_0') ne -1 then begin
                store_data, thx+'_efs_0'+out_suf, data = {x:d.x, y:thx_efs_0}, limit = l, dlimit = dl
                if keyword_set(coord) && strlowcase(coord) ne 'dsl' then begin ;cotrans here, jmm, 2010-02-22
                  thm_cotrans, thx+'_efs_0'+out_suf, out_coord = coord, use_spinaxis_correction = 1, use_spinphase_correction = 1
                endif
              endif

              Ez=(efs[*,0]*fgs[*,0] + efs[*,1]*fgs[*,1])/(-1*fgs[*,2])
              angle=acos(fgs[*,2]/(fgs[*,0]^2+fgs[*,1]^2+fgs[*,2]^2)^.5)*180/!dpi
              angle80=where(angle gt 80)
              if size(angle80,/dim) ne 0 then Ez[where(angle gt 80)]='NaN'
              thx_efs_dot0=efs
              thx_efs_dot0[*,2]=Ez
              str_element, dl, 'ytitle',thx+'_efs_dot0', /add
              if where(dt_output eq 'efs_dot0') ne -1 then begin
                store_data, thx+'_efs_dot0'+out_suf, data = {x:d.x, y:thx_efs_dot0}, limit = l, dlimit = dl
                if keyword_set(coord) && strlowcase(coord) ne 'dsl' then begin ;cotrans here, jmm, 2010-02-22
                  thm_cotrans, thx+'_efs_dot0'+out_suf, out_coord = coord, use_spinaxis_correction = 1, use_spinphase_correction = 1
                endif
              endif

           endif

 ;NOTE: Code below no longer needed as code above has been fixed
           ;correct units, the code above is pretty messy
           ;so rather that sort it out the code below
           ;guarantees the correct units for efs quantities
;
;           idx = where(dt_output eq 'efs' or dt_output eq 'efs_sigma' or dt_output eq 'efs_0' or dt_output eq 'efs_dot0')
;
;           if idx[0] ne -1L then begin
;
;              list = tnames(thx+'_'+dt_output[idx]+out_suf)
;
;              if size(list,/n_dim) ne 0 || list[0] ne '' then begin
;
;                 for cnt = 0,n_elements(list)-1 do begin
;
;                    get_data,list[cnt],dlimits=dl
;
;                    if size(dl, /type) eq 8 then begin ;jmm, feb-8-2008, dl is not always defined for _sigma
;
;                      dl.data_att.units = 'mV/m'
;
;                      dl.ysubtitle = '[mV/m]'
;
;                      dl.ytitle = list[cnt]
;
;                    endif else begin
;
;                      data_att = {units:'mV/m'}
;
;                      dl = {ytitle:list[cnt], ysubtitle:'[mV/m]', data_att:data_att}
;
;                    endelse
;
;                    store_data, list[cnt], dlimits = dl
;
;                 endfor
;              endif
;           endif

        endfor                  ; loop over names.

     endfor                     ; loop over spacecraft.
  endif
end
