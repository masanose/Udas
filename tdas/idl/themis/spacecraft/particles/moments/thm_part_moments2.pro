;+
;PROCEDURE: thm_part_moments2
;PURPOSE: Calculates moments and spectra for themis particle distributions.
;
;SEE ALSO:
;	THM_CRIB_PART_GETSPEC, THM_PART_GETSPEC, THM_PART_GETANBINS, THM_LOAD_SST,
;   THM_LOAD_ESA_PKT, THM_FAC_MATRIX_MAKE,THM_CRIB_SST_CONTAMINATION,
;   THM_SST_REMOVE_SUNPULSE
;
;
;NOTE:
;  For documentation on sun contamination correction keywords that
;  may be passed in through the _extra keyword please see:
;  thm_sst_remove_sunpulse.pro or thm_crib_sst_contamination.pro
;
;MODIFIED BY:	Bryan kerr from Davin Larson's thm_part_moments
;
;  $LastChangedBy: pcruce $
;  $LastChangedDate: 2008-07-21 17:38:17 -0700 (Mon, 21 Jul 2008) $
;  $LastChangedRevision: 3298 $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/spacecraft/particles/moments/thm_part_moments2.pro $
;-


pro thm_part_moments2, instruments_types=instruments, probes=probes,  $  ;moments=moms,  $
                       moments_types=moments_types, $
                       verbose=verbose, $
                       trange=trange, $
                       tplotnames=tplotnames, $
                       tplotsuffix=tplotsuffix, $
                       set_opts=set_opts, $
                       scpot_suffix=scpot_suffix, mag_suffix=mag_suffix, $ ;'inputs: suffix specifying source of magdata and scpot (name - 'th?')
                       comps=comps, get_moments=get_moments,usage=usage, $
                       units=units, theta=theta, phi=phi, pitch=pitch, $
                       erange=erange, start_angle=start_angle, doangle=doangle, $
                       doenergy=doenergy, wrapphi=wrapphi, regrid=regrid, $
                       gyro=gyro, en_tnames=en_tnames, an_tnames=an_tnames, $
                       normalize=normalize, datagap=datagap, $
                     ; gui-related keywords
                       gui_flag=gui_flag, gui_statusBar=gui_statusBar, $
                       gui_historyWin=gui_historyWin, $
                     ; misc keywords
                       _extra=ex, test=test 


if n_elements(gui_flag) eq 0 then gui_flag=0

err_xxx = 0
Catch, err_xxx
IF (err_xxx NE 0) THEN BEGIN
  Catch, /Cancel
  if (err_xxx eq -152) AND gui_flag then begin
    mess = ['Not enough memory to process ' + strupcase(format) + ' . ' + $
            'Try reducing Regrid settings.', $
            'Moving on to next requested Data Type.']
    dum = dialog_message(mess, title='THM_PART_GETSPEC: Insufficient Memory', $
                    /center, /info)
  endif else message, /reissue_last

  RETURN
ENDIF

; begin legacy code from THM_PART_MOMENTS.PRO ==================================
defprobes = '?'
definstruments = 'p??f'
defmoments='density velocity t3 magt3'
start = systime(1)
dtype_prog = 0. ; initialize counter for progress of each data type

vprobes      = ['a','b','c','d','e']
if n_elements(probes) eq 1 then if probes eq 'f' then vprobes=['f']
vinstruments = ['peif','peef','psif','psef','peir','peer','psir','pser','peib','peeb','pseb']
vmoments     = strlowcase(strfilter(tag_names(moments_3d()),['TIME','ERANGE','MASS','VALID'],/negate))

if keyword_set(comps) or keyword_set(get_moments)  then begin
   scp = scope_traceback(/struct)
   nscp = n_elements(scp)
   dprint,'Keyword names have changed. Please change the calling routine: ',scp[nscp-2].routine
   usage=1
endif
if keyword_set(usage) then begin
   scp = scope_traceback(/struct)
   nscp = n_elements(scp)
   dprint,'Typical usage: '  ,scp[nscp-1].routine  ,", INSTRUMENT='pe?f', PROBES='a', MOMENTS='density velocity'"
   dprint,'Valid inputs:'
   dprint,'  PROBES=',"'"+vprobes+"'"
   dprint,'  INSTRUMENTS=',"'"+vinstruments+"'"
   dprint,'  MOMENTS=',"'"+vmoments+"'"
   return
endif

;probea = thm_check_valid_name(size(/type,probes) eq 7 ? probes : '*',vprobes)

probes_a       = strfilter(vprobes,size(/type,probes) eq 7 ? probes : defprobes,/fold_case,delimiter=' ',count=nprobes)
instruments_a  = strfilter(vinstruments,size(/type,instruments)  eq 7 ? instruments  : definstruments,/fold_case,delimiter=' ',count=ninstruments)
moments_a      = strfilter(vmoments,  size(/type,moments_types)  eq 7 ? moments_types  : defmoments  ,/fold_case,delimiter=' ',count=nmoments)
tplotnames=''
if not keyword_set(tplotsuffix) then tplotsuffix=''

;if keyword_set(get_moments) then if not keyword_set(comps)  then comps = ['density','velocity','t3']
;comps = strfilter(vmoments,compmatch,/fold_case,delimiter=' ')
dprint,dlevel=2,/phelp,probes_a
dprint,dlevel=2,/phelp,instruments_a
dprint,dlevel=2,/phelp,moments_a

if size(/type,units) ne 7 then units='eflux'
; end legacy code from THM_PART_MOMENTS.PRO ====================================
facfull = 0 ;initial switch used for energy flux calculation branches
phi1 = phi[0]
phi2 = phi[1]
gyro1 = gyro[0]
gyro2 = gyro[1]

if ~ keyword_set(doangle) then doangle='none'

if ~ keyword_set(datagap) then data_gap=0 else data_gap=datagap

; Setup plotting boundaries for angular plots used by ylim
if keyword_set(start_angle) then begin
   p_start_angle = start_angle
   p_end_angle = start_angle + phi2 + wrapphi - phi1

endif else begin
   start_angle = phi1
   p_start_angle = phi1
   p_end_angle = phi2 + wrapphi
   if doangle eq 'gyro' then begin
      start_angle = gyro1
      p_start_angle = gyro1
      p_end_angle = gyro2 + wrapphi
   endif
endelse
theta1 = theta[0]
theta2 = theta[1]
th_start_angle = theta1
th_end_angle = theta2
pitch1 = pitch[0]
pitch2 = pitch[1]
pa_start_angle = pitch1
pa_end_angle = pitch2

;determine if full pitch/gyro range requested for energy spectra determination
if (pitch2-pitch1) eq 180. AND (gyro2-gyro1) ge 360. then facfull=1

;-----------------------------------------------------------------------

;begin loop over probes
for p = 0,nprobes-1 do begin

    probe= probes_a[p]
    thx = 'th'+probe

    ; begin loop over data_types
    for t=0,ninstruments-1 do begin

        next_type = 0 ; hack to help break out of loop
        instrument = instruments_a(t)
        format = thx+'_'+instrument
        times= thm_part_dist(format,/times) ; store times of data samples in an array
        if size(times,/type) ne 5 then begin
           dprint, 'No ',thx,'_',instrument,' data for time range ',time_string(trange[0]), $
                   ' to ',time_string(trange[1]),'. Continuing on to next data type.'
           
           if gui_flag then begin
             gui_mess = 'No '+thx+'_'+instrument+' data for time range '+ $
                        time_string(trange[0])+' to '+time_string(trange[1])+ $
                        '. Continuing on to next data type.'
             gui_statusBar->update, gui_mess
             gui_historyWin->update, gui_mess
           endif
           print, ''
           continue
        endif 
        
        ;time correction to point at bin center is applied for ESA, but not for SST
        if strmid(instrument,1,1) eq 's' then begin
          times += 1.5
        endif

        ; check if data exists w/in timerange set by user
        if keyword_set(trange) then tr = minmax(time_double(trange)) else tr=[1,1e20]
        ind = where(times ge tr[0] and times le tr[1],ns)

        if ns le 1 then begin ; won't bother with only 1 data point
          mess = 'WARNING: No data for '+ format +' in requested time range.'
          dprint, mess
          if gui_flag then begin
            gui_statusBar->update, mess
            gui_historyWin->update, mess
          endif
        endif else begin
          dprint,format,ns,' elements'
          gui_mess = format + ' ' + string(ns) + ' elements'
        endelse

        if ns gt 1  then begin ; if data exists within timerange set by user
           if keyword_set(mag_suffix) then begin ; interp mag data
                dprint,dlevel=2,verbose=verbose,'Interpolating mag data from: ',thx+mag_suffix
                tinterpol_mxn,thx+mag_suffix,times[ind],/nan_extrapolate,error=error
                if ~error then begin
                  err_mess = 'Error interpolating mag data.'
                  dprint,err_mess
                  if gui_flag then begin
                    gui_statusBar->update, err_mess
                    gui_historyWin->update, err_mess
                  endif
                  return
                endif
           endif
;           if keyword_set(scpot_suffix) then begin ; interp sc potential data
;                dprint,dlevel=2,verbose=verbose,'Interpolating sc potential from:',thx+scpot_suffix
;                scpot = data_cut(thx+scpot_suffix,times[ind])
;           endif
           
           dat = thm_part_dist(format,index=0,_extra=ex)

           if keyword_set(nmoments) then moms = replicate( moments_3d(), ns )
           time = replicate(!values.d_nan,ns)
           maxnrgs = strmid(instrument,1,1) eq 'e' ? 32 : 16
           dprint,dlevel=3,/phelp,maxnrgs
           spec = replicate(!values.f_nan, ns, maxnrgs )
           energy =  replicate(!values.f_nan,ns, maxnrgs )
           ; TODO: double-check the dimension of these arrays
           phispec = replicate(!values.f_nan,ns, 28) ; might have to change the 2nd dim max
           ;thetaspec = replicate(!values.f_nan,ns, 28) ; might have to change the 2nd dim max
           thetaspec = replicate(!values.f_nan,ns, 88) ; might have to change the 2nd dim max
           angs_red = replicate(!values.f_nan, 128) ; max # of reduced angles
           max_angs_red = angs_red
           min_angs_red = angs_red
           last_angarray = 0


           if doangle eq 'pa' || doangle eq 'gyro' || keyword_set(doenergy) then begin ; create new FAC distribution

              nphifac = long(regrid[0])
              nthfac = long(regrid[1])

              n_anbinsfac = nphifac*nthfac
              
              ; check to make sure the xyz array sizes don't exceed 32-bit limit
              if (n_anbinsfac * ns * 3D * 8 gt 2D^31) AND gui_flag then begin
                mess = ['Regrid sizes are too large for amount of time requested.', $
                        strupcase(format) + ' will not be processed.']
                dum = dialog_message(mess, title='THM_PART_GETSPEC: Insufficient Memory', $
                                /center, /info)
                continue
              end

              ; create FAC version of phis, thetas, dphis, dthetas using REGRID input
              dphifac = 360./nphifac
              dthfac = 180./nthfac
              phifac = (indgen(nphifac*nthfac,/float) mod nphifac)*dphifac + dphifac/2
              thfac = fix(indgen(nphifac*nthfac,/float)/nphifac)*dthfac + dthfac/2 - 90
              dphifac = temporary(dphifac)*(fltarr(nphifac*nthfac)+1)
              dthfac = temporary(dthfac)*(fltarr(nphifac*nthfac)+1)
              ;new_domega = dphifac*!pi/180 * (sin(!pi/180*(thfac+dthfac/2)) - sin(!pi/180*(thfac-dthfac/2)))

              ; create tplot variable of interpolated mag data
              mat_name = thx+'_fgs_dsl2fac_mat'
              ; create rotation matrix
              thm_fac_matrix_make, thx+mag_suffix+'_interp', newname=mat_name, $
                                   pos_var_name=thx+'_state_pos',error=error, _extra=ex
              if ~error then begin
                err_mess = 'Error generating field aligned coordinate matrix.'
                dprint, err_mess
                if gui_flag then begin
                  gui_statusBar->update, err_mess
                  gui_historyWin->update, err_mess
                endif
                return
              endif
              
              get_data, mat_name, data=d,limits=l,dlimits=dl

              ; create transpose of rotation matrix so FAC dist can be rotated in DSL coord
              tmat=transpose(d.y,[0,2,1])
              store_data, thx+'_fgs_fac2dsl_mat',data={x:d.x,y:tmat},limits=l,dlimits=dl

              ; create x,y,z vector of FAC angle bin centers
              r = dblarr(n_anbinsfac) + 1
              xyzfac = dblarr(ns,n_anbinsfac,3)
              xyzdsl = xyzfac
              timesfac = dblarr(ns)
              sphere_to_cart,r,thfac,phifac,vec=vec

              ; loop over time to put all FAC angles into a tplot variable
              for i=0L,ns-1 do begin  ; loop over time

                 v_s = size(vec,/dimension)
                 ;calculation is pretty straight forward
                 ;we turn x into an N x 3 x 3 so computation can be done element by element
                 tvec = rebin(vec,v_s[0],v_s[1],v_s[1])
                 newtmat = tmat[i,*,*]
                 newtmat = congrid(newtmat,v_s[0],v_s[1],v_s[1])

                 ;custom multiplication requires rebin to stack vector across rows,
                 ;not columns
                 tvec = transpose(tvec, [0, 2, 1])

                 xyzdsltemp = total(tvec*newtmat,3)

                 xyzdsl[i,*,*] = xyzdsltemp

              endfor ; end loop over time

              del_data, '*_tmp'

              ; convert rotated vectors to spherical coords
              cart_to_sphere, xyzdsl[*,*,0], xyzdsl[*,*,1], xyzdsl[*,*,2], rdsldummy, thdsl, phidsl, ph_0_360=1

              if doangle eq 'pa' || doangle eq 'gyro' then begin
                 phispec = replicate(!values.f_nan,ns, nphifac) ; might have to change the 2nd dim max
                 thetaspec = replicate(!values.f_nan,ns, nthfac) ; might have to change the 2nd dim max
                 ;theta = shift(90-pitch,1)
              endif 

           endif ; doangle eq 'pa' || 'gyro' branch
           ;** end development section for pitch angle tplot variables

           nmode=0L ; initialize number of angle modes in time range
           mindex = 0L ; initialize array of the last time index for each mode
           nangs_red = 0L ; initialize array of # of reduced angles for each mode
    
           enoise_tot = thm_sst_erange_bin_val(thx,instrument,times,_extra=ex)
           mask_tot = thm_sst_find_masking(thx,instrument,ind,_extra=ex)

           newtime = systime(1) ; for progress counter
           
           for i=0L,ns-1  do begin ; loop over time
             dat = thm_part_dist(format,index=ind[i],mask_tot=mask_tot,enoise_tot=enoise_tot,_extra=ex)
  ;quick fix to time offset problems, jmm, 28-apr-2008
             If(size(dat, /type) Eq 8) Then Begin
               mid_time = (dat.time+dat.end_time)/2.0
               jtrange = [dat.time, dat.end_time]
               dat.time = mid_time
               str_element, dat, 'end_time', /delete
               str_element, dat, 'trange', jtrange, /add_replace
             Endif
             mode = dat.mode
             ;temp fix because thm_sst_* doesn't set dat.mode
             ;TODO: make sure this still works correctly for 'er'
             if strmid(instrument,1,1) eq 's' then begin
                if strmid(instrument,1,1) eq 'er' then begin
                   case dat.nbins of
                      1: mode = 1
                      6: mode = 0
                   endcase
                endif else mode = ''
             endif

             ;TODO: give energy dependence to phi here?
             avgphi=average(dat.phi,1)
             avgtheta=average(dat.theta,1)

             nrg = average(dat.energy,2)
             if i eq 0 && doangle eq 'pa' || doangle eq 'gyro' then pa_red = replicate(!values.f_nan,ns,nthfac)
             if i eq 0 && doangle eq 'pa' || doangle eq 'gyro' then paspec = replicate(!values.f_nan,ns,nthfac)
             angarr=[dat.theta,dat.phi,dat.dtheta,dat.dphi]
             
             new_ang_mode = 0 ; reset new_ang_mode flag
             
             if array_equal(last_angarray,angarr) eq 0 then begin   ; Sense change in mode
                new_ang_mode = 1
                last_angarray = angarr
                domega = 0
                ; TODO: store previous mode in temp structure
                nmode++ ; increment number of angle modes w/in timerange
                if nmode gt 1 AND doangle ne 'none' then begin
                   mindex = [mindex, i-1] ; last time index of previous mode
                   ;angspec_temp = angspec[0:i-1,*]
                   angs_red_t = replicate(!values.f_nan, 128) ; max number of reduced angles possible
                   angs_red_t[0:nang_red-1] = ang_red ; array of reduced angles from previous mode
                   angs_red = [angs_red, angs_red_t]
                   
                   max_angs_red_t = replicate(!values.f_nan, 128)
                   max_angs_red_t[0:nang_red-1] = max_ang_red
                   max_angs_red = [max_angs_red, max_angs_red_t]
                   
                   min_angs_red_t = replicate(!values.f_nan, 128)
                   min_angs_red_t[0:nang_red-1] = min_ang_red
                   min_angs_red = [min_angs_red, min_angs_red_t]
                                       
                   nangs_red= [nangs_red, nang_red] ; keep track of # of reduced angles for each mode
                   
                endif
                dprint,verbose=verbose,'Index=',strcompress(i),'.  New mode at: ', $
                      time_string(dat.time), '.  Mode: ', strcompress(mode), ' (', $
                      strcompress(dat.nenergy),'E x', strcompress(dat.nbins),'A)'
                ; renew angle bin maps
                thm_part_getanbins, theta=theta, phi=phi, erange=erange, $
                ;thm_part_getanbins, theta=[-90,90], phi=[0,360], erange=erange, $ ; add this later (if doangle eq 'pa' or 'gyro')
                                   data_type=instrument, en_an_bins=en_an_bins, $
                                   an_bins=an_bins, en_bins=en_bins, nrg=nrg, $
                                   avgphi=avgphi, avgtheta=avgtheta

                if new_ang_mode then begin
                  if total(an_bins) eq 0 then begin
                     err_mess='WARNING: No angle bins turned on for '+instrument+ $
                             ' data at '+time_string(dat.time)
                     dprint, err_mess
                     if gui_flag then begin
                       gui_statusBar->update, err_mess
                       gui_historyWin->update, err_mess
                     endif
                  endif
                  if total(en_bins) eq 0 then begin
                     err_mess='WARNING: No energy bins turned on for '+instrument+ $
                          ' data at '+time_string(dat.time)
                     dprint, err_mess
                     if gui_flag then begin
                       gui_statusBar->update, 'THM_PART_MOMENTS2: '+ err_mess
                       gui_historyWin->update, 'THM_PART_MOMENTS2: '+ err_mess
                     endif
;                     next_type = 1 ; continue to next data type
;                     break
                  endif
                endif

             endif

             dsl_an_bins = an_bins;test code
             dsl_en_an_bins = en_an_bins;test code

             time[i] = dat.time
             dim = size(/dimen,dat.data)
             dim_spec = size(/dimensions,spec)

             if keyword_set(units) then begin
               udat = conv_units(dat,units+'')
               bins = udat.bins
               dim_data = size(/dimensions,udat.data)
               nd = size(/n_dimensions,udat.data)
               dim_bins = size(/dimensions,bins)
               if array_equal(dim_data,dim_bins) eq 0 then begin
                  bins = replicate(1,udat.nenergy) # bins   ; special case for McFadden's 3D structures
               endif
               ; Program caused arithmetic error: Floating illegal operand
               ; whenever there's a zero in total(bins ne 0,2)

               bins = bins * en_an_bins

               ; average flux over all energy bins that are "on" for each angle bin
               asp = nd eq 1 ? udat.data : total(udat.data * (bins ne 0),1) / total(bins ne 0,1) ; avg over all nrg bins
               finite_ind = where(finite(asp),COMPLEMENT=infinite_ind, NCOMPLEMENT=ninfinite)

  ;             if ninfinite gt 0 then begin
  ;                asp[infinite_ind] = 0 ; turning this on produces the zeros in pitch/gyro
  ;             endif                    ; plots when narrow phi/theta/energy ranges used.

               bad_ang = where(finite(total(udat.bins,1),/NAN),n); find sst "sunmasked" bins
               if n gt 0 then asp[bad_ang]=!values.f_nan ; re-insert NaNs for sunmasked" bins

                  ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  ; Test output when average flux is of all angle bins is a constant
                  if keyword_set(test) && test eq 1 then begin
                     asp = asp * 0. + 1e5
                  endif
                  ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                  ; Test #3  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                  ; Test output when average flux is of all angle bins is a constant
                  ; This tests energy spectra
                  if keyword_set(test) && test eq 3 then begin
                     udat.data = udat.data * 0. + 1e5
                  endif
                  ; Test #3  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


               
               if keyword_set(doenergy) then begin
                  if keyword_set(facfull) OR (dat.nbins eq 1) then begin
                  ; create standard data/angle distribution for energy spectra if no pitch/gyro
                  ; limits or distribution is single-angle

                     ; average flux over all angle bins that are "on" for each nrg channel
                     sp = nd eq 1 ? udat.data * bins  : total(udat.data * (bins gt 0),2) / total(bins gt 0,2)
    
                     ; create array of values of nrg bin centers that are are "on" for each nrg channel
                     en = nd eq 1 ? udat.energy * bins : total(udat.energy * (bins gt 0),2) / total(bins gt 0,2)
                     if nd eq 1 then begin
                        sp = (udat.data * (bins gt 0)) / (bins gt 0)
                        en = (udat.energy * (bins gt 0)) / (bins gt 0)
                     endif
                     spec[ i, 0:dim[0]-1] = sp ; en_eflux created here
    
                     ; normalize flux to between 0 and 1
                     if keyword_set(normalize) then spec[i,*] = spec[i,*]/max(spec[i,*],/NAN)
                     
                     energy[ i,0:dim[0]-1] = en ; en_eflux channels for y-axis created here
 
    
                  endif else begin ; create FAC data/angle distribution for energy spectra
 
                     ; select which fac angle bins are turned on
                     thm_part_getanbins, theta=shift(90-pitch,1), phi=gyro, erange=erange, $
                                        data_type=instrument, en_an_bins=fac_en_an_bins, $
                                        an_bins=fac_an_bins, en_bins=fac_en_bins, nrg=nrg, $
                                        avgphi=phifac, avgtheta=thfac

                     if new_ang_mode then begin
                       if total(fac_an_bins) eq 0 then begin

                          err_mess='WARNING: No FAC angle bins turned on for '+instrument+ $
                                  ' data at '+time_string(dat.time)
                          dprint, err_mess
                          if gui_flag then begin
                            gui_statusBar->update, 'THM_PART_MOMENTS2: '+ err_mess
                            gui_historyWin->update, 'THM_PART_MOMENTS2: '+ err_mess
                          endif
                       endif
                       if total(fac_en_bins) eq 0 then begin
                          err_mess='WARNING: No FAC energy bins turned on for '+instrument+ $
                               ' data at '+time_string(dat.time)
                          dprint, err_mess
                          if gui_flag then begin
                            gui_statusBar->update, 'THM_PART_MOMENTS2: '+ err_mess
                            gui_historyWin->update, 'THM_PART_MOMENTS2: '+ err_mess
                          endif
                       endif
                     endif
 
                     fac_en_bins = fix(fac_en_bins*average(udat.bins,2))
                     ;an_bins = fac_an_bins
                     fac_en_an_bins = fix(fac_en_bins#fac_an_bins)
                     facbins_en = fix(fac_en_bins#an_bins) ; facbin for turning on NRG channel labels
 
                     aspfac = fltarr(n_anbinsfac)
                     ;datafac = fac_en_an_bins*0. ; FAC version of energy spectra
                     datafac = fac_en_an_bins*!values.F_NAN ; FAC version of energy spectra
                     nd_fac = size(/n_dimensions,datafac) ; # of dims of FAC data array
                     anatphi=average(udat.phi,1); average native phi
                     anatth=average(udat.theta,1); average native theta
                     anatdphi=average(udat.dphi,1); average native dphi
                     anatdth=average(udat.dtheta,1); average native dtheta
                     natmaxphi = anatphi + anatdphi/2
                     natminphi = anatphi - anatdphi/2
                     natmaxth = anatth + anatdth/2
                     natminth = anatth - anatdth/2
 
                     ; convert any phi's gt 360
                     gt360_ind = where(anatphi gt 360,gt360_count)
                     if gt360_count ne 0 then begin
                        anatphi(gt360_ind) = anatphi(gt360_ind) - 360
                        natmaxphi(gt360_ind) = natmaxphi(gt360_ind) - 360
                        natminphi(gt360_ind) = natminphi(gt360_ind) - 360
                     endif
 
                     mycount=0
                     facbininds=[0] ; array of fac angle bins that successfully sampled native angle bins

                     for j=0L,n_anbinsfac-1 do begin ; loop over fac angle bins
                        for k=0L,udat.nenergy-1 do begin ; loop over energy bins
                           if n_elements(anatth) eq 1 then begin
                              ;aspfac[*] = total(udat.data * (bins ne 0),1) / total(bins ne 0,1)
                              datafac[k,*] = total(udat.data * (bins ne 0),1) / total(bins ne 0,1)
                                 ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                 ; Test output when average flux is of all angle bins is a constant
                                 ;if keyword_set(test) && test eq 1 then begin
                                 ;   aspfac = aspfac * 0. + 1e5
                                 ;endif
                                 ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              continue
                           endif
                           tphidsl = phidsl[i,j]
                           tthdsl = thdsl[i,j]
    ;                       natbinind = where(tphidsl lt natmaxphi AND tphidsl ge natminphi $
    ;                                  AND tthdsl lt natmaxth AND tthdsl ge natminth,n)
                           natbinind = where(tphidsl lt natmaxphi AND tphidsl gt natminphi $
                                             AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                             AND tphidsl le phi2 AND tphidsl ge phi1 $
                                             AND tthdsl le theta2 AND tthdsl ge theta1 ,n)
                           if n eq 1 then begin
                              ;aspfac[j]=asp[natbinind]
                              datafac[k,j] = udat.data[k,natbinind]
                           endif
 
                           if n gt 1 then begin
                              ;aspfac[j] = total(asp[natbinind])/n
                              datafac[k,j] = total(udat.data[k,natbinind])/n
                           endif
    
                           if n eq 0 then begin ; account for wrapping around 360 or 0 degrees in phi
                              wrap360 = where(natmaxphi ge 360.,nwrap360)
                              if nwrap360 gt 0 then begin
                                 wrap360ind = where(tphidsl+360. lt natmaxphi AND tphidsl+360. gt natminphi $
                                                    AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                                    AND tphidsl le phi2 AND tphidsl ge phi1 $
                                                    AND tthdsl le theta2 AND tthdsl ge theta1 ,nwrap360ind)
                              endif else nwrap360ind = 0
    
                              wrap0 = where(natminphi le 0.,nwrap0)
                              if nwrap0 gt 0 then begin
                                 wrap0ind = where(tphidsl-360. lt natmaxphi AND tphidsl-360. gt natminphi $
                                                  AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                                  AND tphidsl le phi2 AND tphidsl ge phi1 $
                                                  AND tthdsl le theta2 AND tthdsl ge theta1 ,nwrap0ind)
                              endif else nwrap0ind = 0
    
                              if nwrap360ind gt 0 then natbinind = wrap360ind
                              if nwrap0ind gt 0 then begin
                                 if nwrap360ind gt 0 then natbinind = [natbinind, wrap0ind] else natbinind = wrap0ind
                              endif
    
                              if nwrap360ind gt 0 OR nwrap0ind gt 0 then begin
                                ;aspfac[j] = average(asp[natbinind])
                                datafac[k,j] = average(udat.data[k,natbinind])
                              endif else begin
                                mycount=mycount+1
                                ;aspfac[j] = !values.f_nan
                                datafac[k,j] = !values.f_nan
                              endelse
    
                           endif
                           
    ;                       if aspfac[j] eq 0.0 then begin
    ;                             print, j
    ;                       endif
 
                        endfor ; loop over energy bins
                        
                        if natbinind ne -1 then facbininds = [facbininds, j]
                        
                     endfor ; loop over fac angle bins
 
    ;                 dprint, 'COUNT = ', mycount
 
                     facbins = fac_en_an_bins
                     if n_elements(facbininds) gt 1 then $
                       facbininds = facbininds[1:n_elements(facbininds)-1] $
                       else facbininds = [0]
                     fac_an_bins_inds = where(fac_an_bins)
                     ; # of native bins that are sampled by fac grid and in requested pitch/gyro ranges
                     fac_ang_on = ssl_set_intersection(facbininds, fac_an_bins_inds)
                     n_fac_ang_on = n_elements(fac_ang_on)
                     if (n_fac_ang_on eq 1) && (fac_ang_on eq -1) then n_fac_ang_on=0
    
                     ; average flux over all angle bins that are "on" for each nrg channel
                     ; divides only by number of fac angle bins that successfully sampled a native dsl bin
                     sp = nd_fac eq 1 ? datafac * facbins : total(datafac * (facbins gt 0),2,/NAN) / n_fac_ang_on / fac_en_bins
                    
                     ; create array of values of nrg bin centers that are are "on" for each nrg channel
                     en = nd eq 1 ? udat.energy * facbins_en : total(udat.energy * (facbins_en gt 0),2) / total(facbins_en gt 0,2)

                     if nd_fac eq 1 then sp = (datafac * (facbins gt 0)) / (facbins gt 0)
                     if nd eq 1 then en = (udat.energy * (facbins_en gt 0)) / (facbins_en gt 0)

                     spec[ i, 0:dim[0]-1] = sp ; en_eflux created here
    
                     ; normalize flux to between 0 and 1
                     if keyword_set(normalize) then spec[i,*] = spec[i,*]/max(spec[i,*],/NAN)
                     
                     energy[ i,0:dim[0]-1] = en ; en_eflux channels for y-axis created here
 
 
                     ;asp = aspfac
                     ;udat_init=fltarr(udat.nenergy)+1
                     ;udatphi = udat_init#phifac
                     ;udattheta = udat_init#thfac
                     ;udatdphi = udat_init#dphifac
                     ;udatdtheta = udat_init#dthfac
 
                  endelse ; create FAC data/angle distribution for energy spectra
               endif ; doenergy


               udatphi = udat.phi
               udattheta = udat.theta
               udatdphi = udat.dphi
               udatdtheta = udat.dtheta

               if doangle eq 'pa' || doangle eq 'gyro' then begin ; create FAC data/angle distribution for pa/gyro spectra

                 ; select which fac angle bins are turned on
                 thm_part_getanbins, theta=shift(90-pitch,1), phi=gyro, erange=erange, $
                                    data_type=instrument, en_an_bins=fac_en_an_bins, $
                                    an_bins=fac_an_bins, en_bins=fac_en_bins, nrg=nrg, $
                                    avgphi=phifac, avgtheta=thfac

                 if new_ang_mode then begin
                   if total(fac_an_bins) eq 0 then begin

                      err_mess='WARNING: No FAC angle bins turned on for '+instrument+ $
                              ' data at '+time_string(dat.time)
                      dprint, err_mess
                      if gui_flag then begin
                        gui_statusBar->update, 'THM_PART_MOMENTS2: '+ err_mess
                        gui_historyWin->update, 'THM_PART_MOMENTS2: '+ err_mess
                      endif
                   endif
                   if total(fac_en_bins) eq 0 then begin
                      err_mess='WARNING: No FAC energy bins turned on for '+instrument+ $
                           ' data at '+time_string(dat.time)
                      dprint, err_mess
                      if gui_flag then begin
                        gui_statusBar->update, 'THM_PART_MOMENTS2: '+ err_mess
                        gui_historyWin->update, 'THM_PART_MOMENTS2: '+ err_mess
                      endif
                   endif
                 endif

                 an_bins = fac_an_bins
                 en_bins = fac_en_bins
                 ;en_an_bins = fac_en_an_bins

                 aspfac = fltarr(n_anbinsfac)
                 anatphi=average(udat.phi,1); average native phi
                 anatth=average(udat.theta,1); average native theta
                 anatdphi=average(udat.dphi,1); average native dphi
                 anatdth=average(udat.dtheta,1); average native dtheta
                 natmaxphi = anatphi + anatdphi/2
                 natminphi = anatphi - anatdphi/2
                 natmaxth = anatth + anatdth/2
                 natminth = anatth - anatdth/2

                 ; convert any phi's gt 360
                 gt360_ind = where(anatphi gt 360,gt360_count)
                 if gt360_count ne 0 then begin
                   anatphi(gt360_ind) = anatphi(gt360_ind) - 360
                   natmaxphi(gt360_ind) = natmaxphi(gt360_ind) - 360
                   natminphi(gt360_ind) = natminphi(gt360_ind) - 360
                 endif

                 mycount=0L


                 for j=0L,n_anbinsfac-1 do begin ; loop over fac angle bins
                   if n_elements(anatth) eq 1 then begin
                     aspfac[*] =total(udat.data * (bins ne 0),1) / total(bins ne 0,1)
                       ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                       ; Test output when average flux is of all angle bins is a constant
                       if keyword_set(test) && test eq 1 then begin
                         aspfac = aspfac * 0. + 1e5
                       endif
                       ; Test #1  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                     continue
                   endif
                   tphidsl = phidsl[i,j]
                   tthdsl = thdsl[i,j]
  ;                 natbinind = where(tphidsl lt natmaxphi AND tphidsl ge natminphi $
  ;                            AND tthdsl lt natmaxth AND tthdsl ge natminth,n)
                   natbinind = where(tphidsl lt natmaxphi AND tphidsl gt natminphi $
                                     AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                     AND tphidsl le phi2 AND tphidsl ge phi1 $
                                     AND tthdsl le theta2 AND tthdsl ge theta1 ,n)
                   if n eq 1 then begin
                     aspfac[j]=asp[natbinind]
                   endif

                   if n gt 1 then begin
                     aspfac[j] = total(asp[natbinind])/n
                   endif

                   if n eq 0 then begin ; account for wrapping around 360 or 0 degrees in phi
                     wrap360 = where(natmaxphi ge 360.,nwrap360)
                     if nwrap360 gt 0 then begin
                       wrap360ind = where(tphidsl+360. lt natmaxphi AND tphidsl+360. gt natminphi $
                                          AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                          AND tphidsl le phi2 AND tphidsl ge phi1 $
                                          AND tthdsl le theta2 AND tthdsl ge theta1 ,nwrap360ind)
                     endif else nwrap360ind = 0

                     wrap0 = where(natminphi le 0.,nwrap0)
                     if nwrap0 gt 0 then begin
                       wrap0ind = where(tphidsl-360. lt natmaxphi AND tphidsl-360. gt natminphi $
                                        AND tthdsl lt natmaxth AND tthdsl ge natminth $
                                        AND tphidsl le phi2 AND tphidsl ge phi1 $
                                        AND tthdsl le theta2 AND tthdsl ge theta1 ,nwrap0ind)
                     endif else nwrap0ind = 0

                     if nwrap360ind gt 0 then natbinind = wrap360ind
                     if nwrap0ind gt 0 then begin
                       if nwrap360ind gt 0 then natbinind = [natbinind, wrap0ind] else natbinind = wrap0ind
                     endif

                     if nwrap360ind gt 0 OR nwrap0ind gt 0 then begin
                       aspfac[j] = average(asp[natbinind])
                     endif else begin
                       mycount=mycount+1
                       aspfac[j] = !values.f_nan
                     endelse

                   endif

   ;                if aspfac[j] eq 0.0 then begin
   ;                      print, j
   ;                endif

                 endfor ; loop over fac angle bins

    ;             dprint, 'COUNT = ', mycount

                 asp = aspfac
                 udat_init=fltarr(udat.nenergy)+1
                 udatphi = udat_init#phifac
                 udattheta = udat_init#thfac
                 udatdphi = udat_init#dphifac
                 udatdtheta = udat_init#dthfac

               endif ; create FAC data/angle distribution for pa/gyro spectra



               ; compute phi angle spectra
               if doangle eq 'phi' || doangle eq 'gyro' then begin

                 ; Test #2  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 ; Test output when average flux is of all angle bins is a constant
                 if keyword_set(test) && test eq 2 then begin
                    ;asp = fltarr(88) + 1e5 ;test remove when done
                    ;asp = fltarr(64) + 1e5 ;test remove when done
                    asp =fltarr(6) + 1e5 ;test remove when done
                    asp[1] = 0
                    ;asp[2:5] = 1e7
                    ;asp[3:4] = 1e3
                    ;asp[0:1] = 0
                    ;aspi1=[0,4,8,9,16,17];,24,40,56,72]
                    ;aspi1=[0,1,2,3,4,5,6,7]
                    ;aspi2=[4,5,6,7]
                    ;asp(0:3) = 1e4
                    ;asp(16:19) = 1e6
                    ;aspi3=[1,2,3,4,10,11,18,19,5,6,7,8,30,62,78,46,31,63,77,47,28,60,76,44]
                    ;asp[aspi3] = 0
                  endif
                 ; Test #2  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

                 asp_on = intarr(n_elements(asp))
                 asp_on_ind = where(asp gt 0,n_asp_on_ind) ; index of angle bin where spectra has been calc'd
  

                 if (total(an_bins) gt 0) && (total(en_bins) gt 0) && (n_asp_on_ind gt 0) then begin
                 ; if angle & nrg bins exist in reqested ranges and spectra exist for 1 or more angle bins
                   an_bins_ind = where(an_bins eq 1)

                   en_binsi = where(en_bins gt 0, n_en_binsi)
                   dphi = average(udatdphi[en_binsi,*],1)
                   dtheta = average(udatdtheta[en_binsi,*],1)

                   asp_on(asp_on_ind) = 1
                   ;aphi = average(udatphi[en_binsi,*],1) ; avg phi of nrgies collected in each angle bin
                   aphi = average(udatphi,1) ; avg phi of all available nrgies in each angle bin
                   ;maxphi = max(udatphi[en_binsi,*],dimension=1, min=minphi) ; min/max phi of energies collected in each angle bin
                   ; use this if you want the min/max phi of each bin regardless of nrgies picked
                   maxphi = max(udatphi,dimension=1, min=minphi)
                   if strmid(instrument,1,1) eq 's' AND doangle eq 'phi' then begin
                     maxphi = aphi + 11 ; based on SST angular response FWHM of ~22 degrees
                     minphi = aphi - 11 ; based on SST angular response FWHM of ~22 degrees
                   endif else begin
                     if array_equal(maxphi, aphi) then begin
                       maxphi = aphi + dphi/4 ;the 4 is arbitrary since angular response not in dist.
                       minphi = aphi - dphi/4 ;the 4 is arbitrary since angular response not in dist.
                     endif
                   endelse
  
                   ; convert any phi's gt 360
                   gt360_ind = where(aphi gt 360,gt360_count)
                   if gt360_count ne 0 then begin
                     aphi(gt360_ind) = aphi(gt360_ind) - 360
                     maxphi(gt360_ind) = maxphi(gt360_ind) - 360
                     minphi(gt360_ind) = minphi(gt360_ind) - 360
                   endif
  
                   atheta = average(udattheta[en_binsi,*],1) ; avg theta of energies collected in each angle bin
                   aphi_on = aphi(an_bins_ind) ; array of aphi reduced to only bins that are on
                   maxphi_on = maxphi(an_bins_ind)
                   minphi_on = minphi(an_bins_ind)
                   atheta_on = atheta(an_bins_ind) ; array of atheta reduced to only bins that are on
                   dphi_on = average(udatdphi(0,an_bins_ind),1)
                   dtheta_on = average(udatdtheta(0,an_bins_ind),1)
                   ; might have to test the fact that SORT might treat identical
                   ; elements differently on different OS's won't affect output
                   phi_dat_ind = uniq(aphi_on, sort(aphi_on)); udatphi bin #'s of unique and sorted phi's
                   aphi_red = aphi_on(phi_dat_ind) ; sort and pick out unique avgphi's
                   maxphi_red = maxphi_on(phi_dat_ind)
                   minphi_red = minphi_on(phi_dat_ind)
                   atheta_red = atheta_on(phi_dat_ind) ; sort and pick out unique avgtheta's
                   dphi_red = dphi_on(phi_dat_ind)
                   nphi = n_elements(aphi_red)
  
                   for a = 0L,nphi-1 do begin
                     ; look for bins that fall in current bin's primary angle range
                     if minphi_red[a] lt 0 then begin
                       combins = where((aphi*an_bins + dphi*an_bins/2) gt minphi_red[a] $
                                        AND (aphi*an_bins - dphi*an_bins/2) lt maxphi_red[a])
                       wrapbin = where((aphi*an_bins + dphi*an_bins/2) ge 360,nwrapbin) ;account for bins with boundary at 360 degrees
                       if nwrapbin gt 0 then begin
                         if (aphi[wrapbin]*an_bins[wrapbin] + $
                           dphi[wrapbin]*an_bins[wrapbin]/2) gt (minphi_red[a] + 360) then begin
                           combins = [combins, wrapbin]
                         endif
                       endif
  
                       large_dphi_ind = where((aphi[combins] + dphi[combins]/2) gt maxphi_red[a] $
                                           AND (aphi[combins] - dphi[combins]/2) lt minphi_red[a], $
                                           nlarge_dphi, COMPLEMENT=small_dphi_ind, $
                                           NCOMPLEMENT=nsmall_dphi)
                     endif else begin
                       combins = where((aphi*an_bins + dphi*an_bins/2) gt minphi_red[a] $
                                        AND (aphi*an_bins - dphi*an_bins/2) lt maxphi_red[a], ncombins)
                       wrapbin = where((aphi*an_bins + dphi*an_bins/2) ge 360,nwrapbin) ;account for bins with boundary at 360 degrees
                       fullbins = where(dphi*an_bins eq 360, nfullbins)
                       if ncombins eq 1 AND nfullbins gt 0 then begin
                         combins = [fullbins, combins]
                         large_dphi_ind = indgen(n_elements(combins))
                         nlarge_dphi = n_elements(large_dphi_ind)
                         nsmall_dphi = 0
                       endif else begin
  
                         large_dphi_ind = where((aphi[combins] + dphi[combins]/2) gt maxphi_red[a] $
                                          AND (aphi[combins] - dphi[combins]/2) lt minphi_red[a], $
                                          nlarge_dphi, COMPLEMENT=small_dphi_ind, $
                                          NCOMPLEMENT=nsmall_dphi)
                       endelse
                     endelse
  
                     if nsmall_dphi gt 0 then begin
                       phispec_b = fltarr(nsmall_dphi)
                       del_omega_rad_b = phispec_b
  
                       for b = 0,nsmall_dphi-1 do begin
                         temp_ind = combins[small_dphi_ind[b]]
                         if strmid(instrument,1,1) eq 's' then begin
                           ;hw = min(dphi_red) / 4 ; fix in absence of sst phi-collection range
                           hw = (maxphi_red[a] - minphi_red[a])/2
                         endif else begin
                           ; use this if avg phi is based on nrgies collected in each angle bin
                           ;hw = (maxphi_red[a] - minphi_red[a] + $
                           ;     (maxphi_red[a] - minphi_red[a]) / (n_en_binsi - 1)) / 2
                           hw = (maxphi_red[a] - minphi_red[a] + $
                                (maxphi_red[a] - minphi_red[a]) / (dat.nenergy - 1)) / 2
                         endelse
                         if aphi_red[a] gt aphi[temp_ind] then begin
                           w = (aphi[temp_ind] + dphi[temp_ind]/2) - (aphi_red[a] - hw)
                         endif else if aphi_red[a] lt aphi[temp_ind] then begin
                           if (aphi[temp_ind] + dphi[temp_ind]) gt aphi_red[a] $
                             AND (aphi[temp_ind] - dphi[temp_ind]) gt aphi_red[a] then begin
                             w = aphi_red[a] + hw + 360 - $
                                 (aphi[temp_ind] + dphi[temp_ind]/2)
                           endif else begin
                             w = (aphi_red[a] + hw) - (aphi[temp_ind] - dphi[temp_ind]/2)
                           endelse
                         endif else begin
                           w = 2 * hw
                         endelse
                         th0_rad = (atheta[temp_ind] + dtheta[temp_ind]/2.)*!pi/180.
                         th1_rad = (atheta[temp_ind] - dtheta[temp_ind]/2.)*!pi/180.
                         w_rad = w * !pi/180.
                         del_omega_rad_b[b] = w_rad * (sin(th0_rad) - sin(th1_rad))
                         if ~ finite(asp[temp_ind]) then del_omega_rad_b[b] = 0.
                         phispec_b[b] = asp[temp_ind] * del_omega_rad_b[b]
                         ;phispec_b[b] = asp[temp_ind] * finite(asp[temp_ind]);testing line
                       endfor
  
                     endif else begin
                       phispec_b = 0.
                       del_omega_rad_b = 0.
                     endelse
                     phispec_c = fltarr(nlarge_dphi)
                     del_omega_rad_c = phispec_c
  
                     for c = 0,nlarge_dphi-1 do begin
                       temp_ind = combins[large_dphi_ind[c]]
                       th0_rad = (atheta[temp_ind] + dtheta[temp_ind]/2.)*!pi/180.
                       th1_rad = (atheta[temp_ind] - dtheta[temp_ind]/2.)*!pi/180.
                       if strmid(instrument,1,1) eq 's' then begin
                         w = maxphi_red[a] - minphi_red[a] ; fix in absence of sst phi-collection range
                       endif else begin
                         ; use this if avg phi is based on nrgies collected in each angle bin
                         ;w = maxphi_red[a] - minphi_red[a] + $
                         ;    (maxphi_red[a] - minphi_red[a]) / (n_en_binsi - 1)
                         w = maxphi_red[a] - minphi_red[a] + $
                             (maxphi_red[a] - minphi_red[a]) / (dat.nenergy - 1)
                       endelse
                       w_rad = w * !pi/180
                       del_omega_rad_c[c] = w_rad * (sin(th0_rad) - sin(th1_rad))
                       if ~ finite(asp[temp_ind]) then del_omega_rad_c[c] = 0.
                       phispec_c[c] = asp[temp_ind] * del_omega_rad_c[c]
                       ;phispec_c[c] = asp[temp_ind] * finite(asp[temp_ind]);testing line
                     endfor
  
                     del_omega_rad2 = total(del_omega_rad_b) + total(del_omega_rad_c)
                     phispec[i,a] = (total(phispec_b,/NAN) + total(phispec_c,/NAN)) / del_omega_rad2
                     ;phispec[i,a] = (total(phispec_b,/NAN) + total(phispec_c,/NAN));testing line
                   endfor ; loop over phi's
  
                 endif else begin ; branch if no spectra in any angle bins
                   ;all bins have zero spectra data
                   if i gt 0 then begin
                     nphi_bins = n_elements(phispec[i-1])
                     phispec[i] = fltarr(nphi_bins)
                   endif else begin
                     phispec[i,*] = 0; the initial size of the phispec array
                     nphi = n_elements(phispec[i,*])
                     aphi_red = fltarr(nphi)
                     dphi_red = fltarr(nphi)
                     maxphi_red = fltarr(nphi)
                     minphi_red = fltarr(nphi)
                   endelse
                 endelse
  
                 ; normalize flux
                 if keyword_set(normalize) then phispec[i,*] = phispec[i,*]/max(phispec[i,*],/NAN)
  
                 angspec = phispec
                 ang_red = aphi_red
                 nang_red = n_elements(ang_red)
                 max_ang_red = maxphi_red
                 min_ang_red = minphi_red
                 ; end compute phi angle spectra





               endif else if doangle eq 'pa' || doangle eq 'theta' then begin
                 ; compute pitch angle spectra
     ;            endif else if doangle eq 'theta' then begin
     ;            ; compute theta angle spectra

      ;           ;test section
      ;           asp = fltarr(88) + 1e5 ;test remove when done
      ;           asp = fltarr(64) + 1e5 ;test remove when done
                 ;asp =fltarr(6) + 1e5 ;test remove when done
                 ;asp[0] = 1e7
                 ;asp[3:4] = 0
      ;           ;aspi1=[0,4,8,9,16,17];,24,40,56,72]
      ;           aspi1=[0,1,2,3,4,5,6,7]
      ;           aspi2=[4,5,6,7]
      ;           asp(0:3) = 1e4
      ;           asp(16:19) = 1e6
      ;           aspi3=[1,2,3,4,10,11,18,19,5,6,7,8,30,62,78,46,31,63,77,47,28,60,76,44]
      ;           asp[aspi3] = 0

                 asp_on = intarr(n_elements(asp))
                 asp_on_ind = where(asp gt 0,n_asp_on_ind) ; index of angle bin where spectra has been calc'd
                 if (total(an_bins) gt 0) && (total(en_bins) gt 0) && (n_asp_on_ind gt 0) then begin
                 ; if angle & nrg bins exist in reqested ranges and spectra exist for 1 or more angle bins
                   an_bins_ind = where(an_bins eq 1, n_an_bins)

                   en_binsi = where(en_bins gt 0, n_en_binsi)
                   dphi = average(udatdphi[en_binsi,*],1)
                   dtheta = average(udatdtheta[en_binsi,*],1)

                   asp_on(asp_on_ind) = 1
                   aphi = average(udatphi,1) ; avg phi of all available nrgies in each angle bin

                   ;maxphi = max(udatphi[en_binsi,*],dimension=1, min=minphi) ; min/max phi of energies collected in each angle bin
                   ; use this if you want the min/max phi of each bin regardless of nrgies picked
                   maxphi = max(udatphi,dimension=1, min=minphi)

                   if strmid(instrument,1,1) eq 's' AND doangle eq 'theta' then begin
                     maxphi = aphi + 11 ; based on SST angular response FWHM of ~22 degrees
                     minphi = aphi - 11 ; based on SST angular response FWHM of ~22 degrees
                   endif else begin
                     if array_equal(maxphi, aphi) then begin
                       maxphi = aphi + dphi/4 ;the 4 is arbitrary since angular response not in dist.
                       minphi = aphi - dphi/4 ;the 4 is arbitrary since angular response not in dist.
                     endif
                   endelse

                   atheta = average(udattheta,1) ; avg theta of energies collected in each angle bin
                   ; use this if you want the min/max phi of each bin regardless of nrgies picked
                   maxtheta = max(udattheta,dimension=1, min=mintheta)
                   if array_equal(maxtheta, atheta) then begin
                     maxtheta = atheta + dtheta/2
                     mintheta = atheta - dtheta/2
                   endif

                   ; convert any phi's gt 360
                   gt360_ind = where(aphi gt 360,gt360_count)
                   if gt360_count ne 0 then begin
                     aphi(gt360_ind) = aphi(gt360_ind) - 360
                     maxphi(gt360_ind) = maxphi(gt360_ind) - 360
                     minphi(gt360_ind) = minphi(gt360_ind) - 360
                   endif

                   aphi_on = aphi(an_bins_ind) ; array of aphi reduced to only bins that are on
                   maxphi_on = maxphi(an_bins_ind)
                   minphi_on = minphi(an_bins_ind)
                   atheta_on = atheta(an_bins_ind) ; array of atheta reduced to only bins that are on
                   maxtheta_on = maxtheta(an_bins_ind)
                   mintheta_on = mintheta(an_bins_ind)
                   dphi_on = average(udatdphi(0,an_bins_ind),1)
                   dtheta_on = average(udatdtheta(0,an_bins_ind),1)
                   ; might have to test the fact that SORT might treat identical
                   ; elements differently on different OS's won't affect output
                   phi_dat_ind = uniq(aphi_on, sort(aphi_on)); udatphi bin #'s of unique and sorted phi's
                   theta_dat_ind = uniq(atheta_on, sort(atheta_on)); udattheta bin #'s of unique and sorted theta's
                   aphi_red = aphi_on(phi_dat_ind) ; sort and pick out unique avgphi's
                   atheta_red = atheta_on(theta_dat_ind) ; sort and pick out unique avgtheta's
                   maxphi_red = maxphi_on(phi_dat_ind)
                   minphi_red = minphi_on(phi_dat_ind)
                   maxtheta_red = maxtheta_on(theta_dat_ind)
                   mintheta_red = mintheta_on(theta_dat_ind)
                   dphi_red = dphi_on(phi_dat_ind)
                   nphi = n_elements(aphi_red)
                   dtheta_red = dtheta_on(theta_dat_ind)
                   ntheta = n_elements(atheta_red)

                   for a = 0L,ntheta-1 do begin
                     ; look for bins that fall in current bin's primary angle range
                     combins = where((atheta*an_bins + dtheta*an_bins/2) gt mintheta_red[a] $
                                      AND (atheta*an_bins - dtheta*an_bins/2) lt maxtheta_red[a])
                     ncombins = n_elements(combins)
                     large_dtheta_ind = where((atheta[combins] + dtheta[combins]) gt maxtheta_red[a] $
                                            AND (atheta[combins] - dtheta[combins]) lt mintheta_red[a], $
                                            nlarge_dtheta, COMPLEMENT=small_dtheta_ind, $
                                            NCOMPLEMENT=nsmall_dtheta)
                     if nsmall_dtheta gt 0 then begin
                       thetaspec_b = fltarr(nsmall_dtheta)
                       del_omega_rad_b = thetaspec_b

                       for b = 0,nsmall_dtheta-1 do begin
                         temp_ind = combins[small_dtheta_ind[b]]
                         if strmid(instrument,1,1) eq 's' then begin
                           ;hw = min(dphi_red) / 4 ; fix in absence of sst phi-collection range
                           hw = (maxphi_red[a] - minphi_red[a])/2
                         endif else begin
                           ; use this if avg phi is based on nrgies collected in each angle bin
                           ;hw = (maxphi_red[a] - minphi_red[a] + $
                           ;     (maxphi_red[a] - minphi_red[a]) / (n_en_binsi - 1)) / 2
                           hw = (maxphi_red[a] - minphi_red[a] + $
                                (maxphi_red[a] - minphi_red[a]) / (dat.nenergy - 1)) / 2
                         endelse
                         if aphi_red[a] gt aphi[temp_ind] then begin
                           w = (aphi[temp_ind] + dphi[temp_ind]/2) - (aphi_red[a] - hw)
                         endif else if aphi_red[a] lt aphi[temp_ind] then begin
                           if (aphi[temp_ind] + dphi[temp_ind]) gt aphi_red[a] $
                             AND (aphi[temp_ind] - dphi[temp_ind]) gt aphi_red[a] then begin
                             w = aphi_red[a] + hw + 360 - $
                                 (aphi[temp_ind] + dphi[temp_ind]/2)
                           endif else begin
                             w = (aphi_red[a] + hw) - (aphi[temp_ind] - dphi[temp_ind]/2)
                           endelse
                         endif else begin
                           w = 2 * hw
                         endelse
                         th0_rad = (atheta[temp_ind] + dtheta[temp_ind]/2.)*!pi/180.
                         th1_rad = (atheta[temp_ind] - dtheta[temp_ind]/2.)*!pi/180.
                         w_rad = w * !pi/180.
                         del_omega_rad_b[b] = w_rad * (sin(th0_rad) - sin(th1_rad))
                         if ~ finite(asp[temp_ind]) then del_omega_rad_b[b] = 0.
                         thetaspec_b[b] = asp[temp_ind] * del_omega_rad_b[b]
                         ;thetaspec_b[b] = asp[temp_ind] * finite(asp[temp_ind]);testing line
                       endfor

                     endif else begin
                       thetaspec_b = 0.
                       del_omega_rad_b = 0.
                     endelse
                     thetaspec_c = fltarr(nlarge_dtheta)
                     del_omega_rad_c = thetaspec_c

                     for c = 0,nlarge_dtheta-1 do begin
                       temp_ind = combins[large_dtheta_ind[c]]
                       th0_rad = (atheta[temp_ind] + dtheta[temp_ind]/2.)*!pi/180.
                       th1_rad = (atheta[temp_ind] - dtheta[temp_ind]/2.)*!pi/180.
                       if strmid(instrument,1,1) eq 's' then begin
      ;                   w = maxphi_red[a] - minphi_red[a] ; fix in absence of sst phi-collection range
                         w = maxphi_red[c] - minphi_red[c] ; fix in absence of sst phi-collection range
                       endif else begin
      ;                  w = maxphi_red[a] - minphi_red[a] + $
      ;                       (maxphi_red[a] - minphi_red[a]) / (n_en_binsi - 1)
                         ; use this if avg phi is based on nrgies collected in each angle bin
                         ;w = maxphi_red[c] - minphi_red[c] + $
                         ;    (maxphi_red[c] - minphi_red[c]) / (n_en_binsi - 1)
                         w = maxphi_red[c] - minphi_red[c] + $
                             (maxphi_red[c] - minphi_red[c]) / (dat.nenergy - 1)
                       endelse
                       w_rad = w * !pi/180
                       del_omega_rad_c[c] = w_rad * (sin(th0_rad) - sin(th1_rad))
                       if ~ finite(asp[temp_ind]) then del_omega_rad_c[c] = 0.
                       thetaspec_c[c] = asp[temp_ind] * del_omega_rad_c[c]
                       ;thetaspec_c[c] = asp[temp_ind] * finite(asp[temp_ind]);testing line
                     endfor

                     del_omega_rad2 = total(del_omega_rad_b) + total(del_omega_rad_c)
                     thetaspec[i,a] = (total(thetaspec_b,/NAN) + total(thetaspec_c,/NAN)) / del_omega_rad2
                     ;thetaspec[i,a] = (total(thetaspec_b,/NAN) + total(thetaspec_c,/NAN));testing line
                   endfor ; loop over theta's

                 endif else begin
                   ;all bins have zero spectra data
                   if i gt 0 then begin
                     ntheta_bins = n_elements(thetaspec[i-1,*])
                     thetaspec[i,*] = fltarr(ntheta_bins)
                   endif else begin
                     thetaspec[i,*] = 0; the initial size of the thetaspec array
                     ntheta = n_elements(thetaspec[i,*])
                     atheta_red = fltarr(ntheta)
                     dtheta_red = fltarr(ntheta)
                     maxtheta_red = fltarr(ntheta)
                     mintheta_red = fltarr(ntheta)
                   endelse
                 endelse

                 ; normalize flux
                 if keyword_set(normalize) then thetaspec[i,*] = thetaspec[i,*]/max(thetaspec[i,*],/NAN)

                 angspec = thetaspec
                 ang_red = atheta_red
                 nang_red = n_elements(ang_red)
                 max_ang_red = maxtheta_red
                 min_ang_red = mintheta_red

                 if doangle eq 'pa' then begin ; convert to pitch angle
                   finspec = where(finite(thetaspec[i,*]),complement=infspec,ncomplement=ninfspec)
                   real_nan = where(finite(thetaspec[i,*], /NAN, sign=-1), nreal_nan) ;look for negative NaNs created
                   if nreal_nan gt 0 then ninfspec = ninfspec - nreal_nan             ;where thetaspec[i,a] is written
                   if ninfspec gt 0 AND ntheta lt nthfac then begin

                     blanks = replicate(!values.f_nan,ninfspec)
                     p_angleb = [90-atheta_red,blanks]
                     pa_sort_indb = sort(p_angleb)
                     pa_redb = p_angleb[pa_sort_indb]
                     npab = n_elements(pa_redb)

                     p_angle = 90-atheta_red
                     pa_sort_ind = sort(p_angle)
                     pa_red = p_angle[pa_sort_ind]
                     npa = n_elements(pa_red)
                    
                     paspec[i,*] = thetaspec[i,pa_sort_indb]
                     
                   endif else begin

                     p_angle = 90-atheta_red
                     pa_sort_ind = sort(p_angle)
                     pa_red = p_angle[pa_sort_ind]
                     npa = n_elements(pa_red)
                     npab = npa
                     paspec[i,0:n_elements(pa_sort_ind)-1] = thetaspec[i,pa_sort_ind]
                   endelse

                   angspec=paspec
                   ;TODO: might have to fix this for stitching as well.

                 endif

                 ; end compute theta angle spectra
                 ; end compute pitch angle spectra testing version


               endif else if doangle eq 'none' then begin
                 angspec = 0
;                 continue


               endif else begin
                 dprint, 'ERROR: ', doangle, ' is not a valid spectrogram
                 doangle = 'none'
               endelse



             endif ; keyword_set(units)
     ;        if  nmoments ne 0 then begin
     ;;          dat.dead = 0
     ;          if keyword_set(magf) then  dat.magf=magf[i,*]
     ;          if keyword_set(scpot) then dat.sc_pot=scpot[i]
     ;          moms[i] = moments_3d( dat , domega=domega )
     ;        endif

           ; begin progress counter             
             newtime = systime(1)
             if ~keyword_set(lasttime) then lasttime = newtime
             
             if 10. gt (newtime-lasttime) then continue
             dtype_prog = float(i)/ns
             dtype_prog_msg = format+' is ' + strcompress(string(long(100*dtype_prog)),/remove) $
                              + '% done.'
             if gui_flag then gui_statusBar->update, dtype_prog_msg
             dprint, dtype_prog_msg
             ;dprint,dwait=10.,format,i,'/',strcompress(ns);,'    ',time_string(dat.time)   ; update every 10 seconds
             
             lasttime = newtime
           ; end progress counter
           endfor ; end loop over time

           if next_type then continue

           
           if doangle eq 'phi' || doangle eq 'theta' then begin
              finite_ind = where(finite(angspec),COMPLEMENT=infinite_ind, NCOMPLEMENT=ninfinite)
              if ninfinite gt 0 then begin
                 angspec[infinite_ind] = 0
              endif
           endif
              
           if doangle eq 'phi' || doangle eq 'theta' || doangle eq 'gyro' then begin   
              ; setup for stitching different modes together
              mindex = [mindex, i-1] ; last time index of previous mode
              angs_red_t = replicate(!values.f_nan, 128) ; max number of reduced angles possible
              angs_red_t[0:nang_red-1] = ang_red ; array of reduced angles from previous mode
              angs_red = [angs_red, angs_red_t]
              nangs_red= [nangs_red, nang_red]
              maxangs_red = max(nangs_red) ; max number of reduce angles
              
              max_angs_red_t = replicate(!values.f_nan, 128)
              max_angs_red_t[0:nang_red-1] = max_ang_red
              max_angs_red = [max_angs_red, angs_red_t]
                            
              min_angs_red_t = replicate(!values.f_nan, 128)
              min_angs_red_t[0:nang_red-1] = min_ang_red
              min_angs_red = [min_angs_red, angs_red_t]
              
             
              angs_red = reform(temporary(angs_red),128,nmode+1)
              angs_red = transpose(temporary(angs_red))
              
              max_angs_red = reform(temporary(max_angs_red),128,nmode+1)
              max_angs_red = transpose(temporary(max_angs_red))
              
              min_angs_red = reform(temporary(min_angs_red),128,nmode+1)
              min_angs_red = transpose(temporary(min_angs_red))
             
              angspect = angspec ; create copy of angspec
              ; end setup for stitching different modes together
           endif


           ;test section
           ;angspec[*,10]=0
           ;angspec[1,3]=0.1
           ;end test section

           if not keyword_set(no_tplot) then begin
              prefix = thx+'_'+instrument+'_'
              suffix = '_'+strlowcase(units)+tplotsuffix
              if keyword_set(units) then begin
                 ; beg create tplot vars for energy spectra
                 if keyword_set(doenergy) then begin
                   en_tname = prefix+'en'+suffix
                   spec_ind = where(total(spec,1,/NAN) gt 0,n)

                   if n eq 0 then begin
                   ; no data. spec array is empty
                     err_msg = 'No '+strupcase(en_tname)+' data for current settings.'
                     dprint, err_msg
                     if gui_flag then begin
                       gui_statusBar->update, err_msg
                       gui_historyWin->update, 'THM_PART_MOMENTS2: ' + err_msg
                     endif
                     continue
                   endif

                   if n eq 1 then begin
                   ; spec array has only one element
                      store_data,en_tname, data={x:time, y:spec[*,spec_ind], v:energy[*,spec_ind]},$
                                 dlim={spec:1,zlog:1,ylog:1,datagap:data_gap}
                      ;options,en_tname, 'max_gap_interp',30,/def
                   endif else begin
                      store_data,en_tname, data={x:time, y:spec[*,spec_ind[0]:spec_ind[n-1]], $
                                 v:energy[*,spec_ind[0]:spec_ind[n-1]]}, dlim={spec:1,zlog:1,ylog:1, $
                                 datagap:data_gap}
                   endelse

                   if size(en_tnames,/type) eq 0 then en_tnames = en_tname $
                      else en_tnames = [en_tnames, en_tname]
                 endif ; end create tplot vars for energy spectra


                 ; beg create tplot vars for phi angle spectra
                 if doangle eq 'phi' || doangle eq 'gyro' then begin
                   an_tname = prefix + 'an_eflux_' + doangle + tplotsuffix
                   ; beg re-sort of phi's so they go from 0-360
                   
                   ; beg mode stitching code
                   fbnd_angspec = replicate(!values.f_nan,i,maxangs_red + 2)
                   nang_fbnd_angspec = n_elements(fbnd_angspec[0,*]) ; # of angles in final boundary angspec
                   fbnd_aphi_red = fbnd_angspec
                   aphi_redt = aphi_red
                   
                   for m=1,nmode do begin ; loop over angle modes
                     
                     aphi_red = angs_red[m,0:nangs_red[m]-1]
                     maxphi_red = max_angs_red[m,0:nangs_red[m]-1]
                     minphi_red = min_angs_red[m,0:nangs_red[m]-1]
                     nphi = nangs_red[m]
                     mns = mindex[m] - mindex[m-1]         ; number of samples for each mode
                     if m eq 1 then mns = mns + 1          ; account for first time index number = 0
                     ;angspect = angspec[mindex[m-1]:mindex[m],*] ; create sep. angspec 4 each mode
                     angspect = angspec[mindex[m-1]+1:mindex[m],0:nphi-1] ; create sep. angspec 4 each mode
                     if m eq 1 then angspect = angspec[mindex[m-1]:mindex[m],0:nphi-1] ; create sep. angspec 4 each mode
                     ; end mode stitching code
                     
                     ps_size = n_elements(angspect[0,*])
                     if ps_size gt nphi then begin
                        angspect = angspect[*,0:nphi-1]
                        ps_size = n_elements(angspect[0,*])
                     endif
  
                     if keyword_set(start_angle) then begin
                        if start_angle lt 0 then begin
                           aphi_red = aphi_red - 360
                           st_ind = min(where(start_angle lt aphi_red))
                           aphi_red = shift(aphi_red, -st_ind)
                           too_low_ind = where(start_angle gt aphi_red)
                           aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                        endif else begin
                           st_ind = min(where(start_angle le aphi_red))
                           aphi_red = shift(aphi_red, -st_ind)
                           too_low_ind = where(start_angle gt aphi_red,n)
                           if n gt 0 then aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                           ;aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                        endelse
                     endif else st_ind = 0
  
                     if nd gt 1 then begin
                        if size(/n_dimensions,angspect) eq 1 then begin
                           angspect = reform(angspect,n_elements(angspect),1)
                        endif else begin
                           angspect = shift(angspect, 0, -st_ind)
                        endelse
                     endif
  
                     nwrap = 0
                     ; begin append wrapped phi bins
                     wrap_ind = where((aphi_red gt start_angle) AND $
                                     (aphi_red lt (start_angle + wrapphi)),nwrap)
                     
                     if (wrapphi gt 0) AND (nwrap gt 0) then begin
;                     if wrapphi gt 0 then begin
;                        wrap_ind = where((aphi_red gt start_angle) AND $
;                                        (aphi_red lt (start_angle + wrapphi)),nwrap)
                        wrap_angspec = replicate(0.,ns, ps_size+nwrap)
                        wrap_angspec[*,0:nphi-1] = angspect
                        wrap_angspec[*,nphi:nphi + nwrap-1] = angspect[*,0:nwrap-1]
  
                        wrap_ps_size = n_elements(wrap_angspec[0,*])
                        wrap_aphi_red = replicate(0.,wrap_ps_size)
                        wrap_aphi_red[0:nphi-1] = aphi_red
                        wrap_aphi_red[nphi:nphi + nwrap-1] = aphi_red[0:nwrap-1] + 360
  
                        bnd_angspec = replicate(0.,mns,wrap_ps_size+2)
                        bnd_angspec[*,1:wrap_ps_size] = wrap_angspec
                        bnd_angspec[*,0] = angspect[*,ps_size-1]; might have to make this zeros or NaNs
                        bnd_angspec[*,wrap_ps_size+1] = angspect[*,nwrap] ; what if angspect[*,nwrap] doesn't exist?
  
                        bnd_aphi_red = replicate(0.,wrap_ps_size+2)
                        bnd_aphi_red[1:wrap_ps_size] = wrap_aphi_red
                        bnd_aphi_red[0] = aphi_red[ps_size-1] - 360
                        bnd_aphi_red[wrap_ps_size+1] = aphi_red[nwrap] + 360
                        
                        ; begin stitching code
                        nbnd_angs_red = n_elements(bnd_aphi_red)
                        if nang_fbnd_angspec lt nbnd_angs_red then begin
                           ; enlarge angle dimension of final boundary angspec
                           fbnd_angspect = replicate(!values.f_nan, i, nbnd_angs_red) ; create temp var
                           fbnd_aphi_redt = fbnd_angspect
                           fbnd_angspect[*,0:nang_fbnd_angspec-1] = fbnd_angspec        ; fill temp var
                           ;fbnd_aphi_redt[*,0:nang_fbnd_angspec-1] = bnd_aphi_red
                           fbnd_aphi_redt[*,0:nang_fbnd_angspec-1] = fbnd_aphi_red
                           fbnd_angspec = temporary(fbnd_angspect)            ; fill resized angspec array
                           fbnd_aphi_red = temporary(fbnd_aphi_redt)
                           nang_fbnd_angspec = nbnd_angs_red                  ; resize nang_fbnd_angspec
                        endif
                        ; end stitching code
                     endif else begin ; begin append wrapped phi bins if necceary
                        if (phi[1] - phi[0]) lt 360 then begin
                           bnd_angspec = angspect
                           bnd_aphi_red = aphi_red
                           ;TODO
                           p_start_angle = aphi_red[0] ; might have to add '- dphi_red[0]/2
                           p_end_angle = aphi_red[nphi-1] ; might have to add '+ dphi_red[nphi-1]/2
                           if n_elements(angspect[0,*]) eq 1 then begin
                              ;p_start_angle = minphi_red
                              ;p_end_angle = maxphi_red
                              bnd_angspec = replicate(0.,mns,ps_size+nwrap+2)
                              bnd_angspec[*,1:nphi] = angspect
                              bnd_angspec[*,0] = angspect
                              bnd_angspec[*,nphi+1] = angspect
                              ; end get y boundaries correct
                              ; look for phi angles gt 360 and wrap around to 0
                              bnd_aphi_red = replicate(0.,ps_size+2)
                              bnd_aphi_red[1:nphi] = aphi_red
                              bnd_aphi_red[0] = minphi_red
                              bnd_aphi_red[nphi+1] = maxphi_red
                           endif
                        endif else begin
                           ; begin get y boundaries correct
                           bnd_angspec = replicate(0.,mns,ps_size+nwrap+2)
                           bnd_angspec[*,1:nphi] = angspect
                           bnd_angspec[*,0] = angspect[*,nphi-1]
                           bnd_angspec[*,nphi+1] = angspect[*,0]
                           ; end get y boundaries correct
                           ; look for phi angles gt 360 and wrap around to 0
                           bnd_aphi_red = replicate(0.,ps_size+2)
                           bnd_aphi_red[1:nphi] = aphi_red
                           bnd_aphi_red[0] = aphi_red[nphi-1]-360
                           bnd_aphi_red[nphi+1] = aphi_red[0]+360
                        endelse
                     endelse
                     nbnd_angs_red = n_elements(bnd_aphi_red)
;                     if nang_fbnd_angspec lt nbnd_angs_red then begin
;                        ; enlarge angle dimension of final boundary angspec
;                        fbnd_angspect = replicate(!values.f_nan, i, nbnd_angs_red) ; create temp var
;                        fbnd_aphi_redt = fbnd_angspect
;                        fbnd_angspect[*,0:nang_fbnd_angspec-1] = fbnd_angspec        ; fill temp var
;                        fbnd_aphi_redt[*,0:nang_fbnd_angspec-1] = bnd_aphi_red
;                        fbnd_angspec = temporary(fbnd_angspect)            ; fill resized angspec array
;                        fbnd_aphi_red = temporary(fbnd_aphi_redt)
;                        nang_fbnd_angspec = nbnd_angs_red                  ; resize nang_fbnd_angspec
;                     endif
                     if m eq 1 then begin
                         fbnd_angspec[mindex[m-1]:mindex[m], 0:nbnd_angs_red-1] = bnd_angspec ; create final bnd_angspec
                         fbnd_aphi_red[mindex[m-1]:mindex[m], 0:nbnd_angs_red-1] = replicate(1,mns)#bnd_aphi_red ; create final bnd_aphi_red
                     endif else begin
                         fbnd_angspec[mindex[m-1]+1:mindex[m], 0:nbnd_angs_red-1] = bnd_angspec ; create final bnd_angspec
                         fbnd_aphi_red[mindex[m-1]+1:mindex[m], 0:nbnd_angs_red-1] = replicate(1,mns)#bnd_aphi_red ; create final bnd_aphi_red
                     endelse
                   endfor ; loop over angle modes
                   store_data,an_tname, data= {x:time, y:fbnd_angspec ,v:fbnd_aphi_red}, $
                              dlim={spec:1,zlog:1,ylog:0,datagap:data_gap}
                   ylim,an_tname,p_start_angle, p_end_angle,log=0,/default
                 endif ; end create tplot vars for phi angle spectra



                 ; beg create tplot vars for theta angle spectra
                 if doangle eq 'theta' then begin
                   an_tname = prefix + 'an_eflux_' + doangle + tplotsuffix
                   ; beg re-sort of theta's so they go from -90 to 90
                   
                   ; beg mode stitching code
                   fbnd_angspec = replicate(!values.f_nan,i,maxangs_red + 2)
                   nang_fbnd_angspec = n_elements(fbnd_angspec[0,*]) ; # of angles in final boundary angspec
                   fbnd_atheta_red = fbnd_angspec
                   atheta_redt = atheta_red
                   
                   for m=1,nmode do begin ; loop over angle modes
                     
                     atheta_red = angs_red[m,0:nangs_red[m]-1]
                     maxtheta_red = max_angs_red[m,0:nangs_red[m]-1]
                     mintheta_red = min_angs_red[m,0:nangs_red[m]-1]
                     ntheta = nangs_red[m]
                     mns = mindex[m] - mindex[m-1]         ; number of samples for each mode
                     if m eq 1 then mns = mns + 1          ; account for first time index number = 0
                     ;angspect = angspec[mindex[m-1]:mindex[m],*] ; create sep. angspec 4 each mode
                     angspect = angspec[mindex[m-1]+1:mindex[m],0:ntheta-1] ; create sep. angspec 4 each mode
                     ;if m eq 1 then angspect = angspec[mindex[m-1]:mindex[m],0:nphi-1] ; create sep. angspec 4 each mode
                     if m eq 1 then angspect = angspec[mindex[m-1]:mindex[m],0:ntheta-1] ; create sep. angspec 4 each mode
                     ; end mode stitching code
                   
                     ps_size = n_elements(angspect[0,*])
                     if ps_size gt ntheta then begin
                        angspect = angspect[*,0:ntheta-1]
                        ps_size = n_elements(angspect[0,*])
                     endif
  
                     if 0 then begin ;keyword_set(start_angle) then begin
                        if start_angle lt 0 then begin
                           aphi_red = aphi_red - 360
                           st_ind = min(where(start_angle lt aphi_red))
                           aphi_red = shift(aphi_red, -st_ind)
                           too_low_ind = where(start_angle gt aphi_red)
                           aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                        endif else begin
                           st_ind = min(where(start_angle lt aphi_red))
                           aphi_red = shift(aphi_red, -st_ind)
                           too_low_ind = where(start_angle gt aphi_red,n)
                           if n gt 0 then aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                           ;aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                        endelse
                     endif else st_ind = 0
  
                     if nd gt 1 then begin
                        if size(/n_dimensions,angspect) eq 1 then begin
                           angspect = reform(angspect,n_elements(angspect),1)
                        endif else begin
                           angspect = shift(angspect, 0, -st_ind)
                        endelse
                     endif
  
                     nwrap = 0
                     ; begin append wrapped theta bins
  ;                     wrap_ind = where((aphi_red gt start_angle) AND $
  ;                                     (aphi_red lt (start_angle + wrapphi)),nwrap)
  ;                     
  ;                     if (wrapphi gt 0) AND (nwrap gt 0) then begin
  ;;                     if wrapphi gt 0 then begin
  ;;                        wrap_ind = where((aphi_red gt start_angle) AND $
  ;;                                        (aphi_red lt (start_angle + wrapphi)),nwrap)
  ;                      wrap_angspec = replicate(0.,ns, ps_size+nwrap)
  ;                      wrap_angspec[*,0:nphi-1] = angspec
  ;                      wrap_angspec[*,nphi:nphi + nwrap-1] = angspec[*,0:nwrap-1]
  ;
  ;                      wrap_ps_size = n_elements(wrap_angspec[0,*])
  ;                      wrap_aphi_red = replicate(0.,wrap_ps_size)
  ;                      wrap_aphi_red[0:nphi-1] = aphi_red
  ;                      wrap_aphi_red[nphi:nphi + nwrap-1] = aphi_red[0:nwrap-1] + 360
  ;
  ;                      bnd_angspec = replicate(0.,ns,wrap_ps_size+2)
  ;                      bnd_angspec[*,1:wrap_ps_size] = wrap_angspec
  ;                      bnd_angspec[*,0] = angspec[*,ps_size-1]; might have to make this zeros or NaNs
  ;                      bnd_angspec[*,wrap_ps_size+1] = angspec[*,nwrap] ; what if angspec[*,nwrap] doesn't exist?
  ;
  ;                      bnd_aphi_red = replicate(0.,wrap_ps_size+2)
  ;                      bnd_aphi_red[1:wrap_ps_size] = wrap_aphi_red
  ;                      bnd_aphi_red[0] = aphi_red[ps_size-1] - 360
  ;                      bnd_aphi_red[wrap_ps_size+1] = aphi_red[nwrap] + 360
  ;                   endif else begin ; begin append wrapped theta bins if necceary
                        if (theta[1] - theta[0]) lt 180 then begin
                           bnd_angspec = angspect
                           bnd_atheta_red = atheta_red
                           ;th_start_angle = theta1 ; atheta_red[0]
                           ;th_end_angle = theta2 ; atheta_red[ntheta-1]
                           th_start_angle = atheta_red[0] - dtheta_red[0]/2
                           th_end_angle = atheta_red[ntheta-1] + dtheta_red[ntheta-1]/2
                           if n_elements(angspect[0,*]) eq 1 then begin
                              ;th_start_angle = mintheta_red
                              ;th_end_angle = maxtheta_red
                              bnd_angspec = replicate(0.,mns,ps_size+nwrap+2)
                              bnd_angspec[*,1:ntheta] = angspect
                              bnd_angspec[*,0] = angspect
                              bnd_angspec[*,ntheta+1] = angspect
                              ; end get y boundaries correct
                              ; look for phi angles gt 90 and wrap around to -90
                              bnd_atheta_red = replicate(0.,ps_size+2)
                              bnd_atheta_red[1:ntheta] = atheta_red
                              bnd_atheta_red[0] = mintheta_red
                              bnd_atheta_red[ntheta+1] = maxtheta_red
                           endif
                        endif ;else begin
                           ; begin get y boundaries correct
                           bnd_angspec = replicate(0.,mns,ps_size+nwrap+2)
                           bnd_angspec[*,1:ntheta] = angspect
                           bnd_angspec[*,0] = angspect[*,ntheta-1]
                           bnd_angspec[*,ntheta+1] = angspect[*,0]
                           ; end get y boundaries correct
                           ; look for theta angles gt 90 and wrap around to -90
                           bnd_atheta_red = replicate(0.,ps_size+2)
                           bnd_atheta_red[1:ntheta] = atheta_red
                           bnd_atheta_red[0] = atheta_red[ntheta-1]-180
                           bnd_atheta_red[ntheta+1] = atheta_red[0]+180
                       ; endelse
                     ;endelse
                     nbnd_angs_red = n_elements(bnd_atheta_red)
                     if m eq 1 then begin
                         fbnd_angspec[mindex[m-1]:mindex[m], 0:nbnd_angs_red-1] = bnd_angspec ; create final bnd_angspec
                         fbnd_atheta_red[mindex[m-1]:mindex[m], 0:nbnd_angs_red-1] = replicate(1,mns)#bnd_atheta_red ; create final bnd_aphi_red
                     endif else begin
                         fbnd_angspec[mindex[m-1]+1:mindex[m], 0:nbnd_angs_red-1] = bnd_angspec ; create final bnd_angspec
                         fbnd_atheta_red[mindex[m-1]+1:mindex[m], 0:nbnd_angs_red-1] = replicate(1,mns)#bnd_atheta_red ; create final bnd_aphi_red
                     endelse
                   endfor ; loop over angle modes                   
                   store_data,an_tname, data= {x:time, y:fbnd_angspec ,v:fbnd_atheta_red}, $
                              dlim={spec:1,zlog:1,ylog:0,datagap:data_gap}
                   if theta1 gt th_start_angle then th_start_angle = theta1
                   if theta2 lt th_end_angle then th_end_angle = theta2
                   ylim, an_tname, th_start_angle, th_end_angle,log=0,/default
                 endif ; end create tplot vars for theta angle spectra




                 ; beg create tplot vars for pitch angle angle spectra
                 if doangle eq 'pa' then begin
                   an_tname = prefix + 'an_eflux_' + doangle + tplotsuffix
                   ; beg re-sort of pa's so they go from -90 to 90
                   ps_size = n_elements(angspec[0,*])
                   if ps_size gt npab then begin
                      angspec = angspec[*,0:npab-1]
                      ps_size = n_elements(angspec[0,*])
                   endif

                   if 0 then begin ;keyword_set(start_angle) then begin
                      if start_angle lt 0 then begin
                         aphi_red = aphi_red - 360
                         st_ind = min(where(start_angle lt aphi_red))
                         aphi_red = shift(aphi_red, -st_ind)
                         too_low_ind = where(start_angle gt aphi_red)
                         aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                      endif else begin
                         st_ind = min(where(start_angle lt aphi_red))
                         aphi_red = shift(aphi_red, -st_ind)
                         too_low_ind = where(start_angle gt aphi_red,n)
                         if n gt 0 then aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                         ;aphi_red[too_low_ind] = aphi_red[too_low_ind] + 360
                      endelse
                   endif else st_ind = 0

                   if nd gt 1 then begin
                      if size(/n_dimensions,angspec) eq 1 then begin
                         angspec = reform(angspec,n_elements(angspec),1)
                      endif else begin
                         angspec = shift(angspec, 0, -st_ind)
                      endelse
                   endif

                   nwrap = 0
                   ; begin append wrapped theta bins
;                     wrap_ind = where((aphi_red gt start_angle) AND $
;                                     (aphi_red lt (start_angle + wrapphi)),nwrap)
;                     
;                     if (wrapphi gt 0) AND (nwrap gt 0) then begin
;;                     if wrapphi gt 0 then begin
;;                        wrap_ind = where((aphi_red gt start_angle) AND $
;;                                        (aphi_red lt (start_angle + wrapphi)),nwrap)
;                      wrap_angspec = replicate(0.,ns, ps_size+nwrap)
;                      wrap_angspec[*,0:nphi-1] = angspec
;                      wrap_angspec[*,nphi:nphi + nwrap-1] = angspec[*,0:nwrap-1]
;
;                      wrap_ps_size = n_elements(wrap_angspec[0,*])
;                      wrap_aphi_red = replicate(0.,wrap_ps_size)
;                      wrap_aphi_red[0:nphi-1] = aphi_red
;                      wrap_aphi_red[nphi:nphi + nwrap-1] = aphi_red[0:nwrap-1] + 360
;
;                      bnd_angspec = replicate(0.,ns,wrap_ps_size+2)
;                      bnd_angspec[*,1:wrap_ps_size] = wrap_angspec
;                      bnd_angspec[*,0] = angspec[*,ps_size-1]; might have to make this zeros or NaNs
;                      bnd_angspec[*,wrap_ps_size+1] = angspec[*,nwrap] ; what if angspec[*,nwrap] doesn't exist?
;
;                      bnd_aphi_red = replicate(0.,wrap_ps_size+2)
;                      bnd_aphi_red[1:wrap_ps_size] = wrap_aphi_red
;                      bnd_aphi_red[0] = aphi_red[ps_size-1] - 360
;                      bnd_aphi_red[wrap_ps_size+1] = aphi_red[nwrap] + 360
;                   endif else begin ; begin append wrapped theta bins if necceary
                      ;if (pitch[1] - pitch[0]) le 180 then begin
                         bnd_angspec = angspec
                         bnd_pa_red = pa_red
                         th_start_angle = pitch1 ; pa_red[0] - dthfac[0]/2
                         th_end_angle = pitch2 ; pa_red[npa-1] + dthfac[0]/2
                         if n_elements(angspec[0,*]) eq 1 then begin
                            th_start_angle = pa_red - 0.5 ; arbitrary bounds
                            th_end_angle = pa_red + 0.5 ; arbitrary bounds
                            bnd_angspec = replicate(0.,ns,ps_size+nwrap+2)
                            bnd_angspec[*,1:npa] = angspec
                            bnd_angspec[*,0] = angspec
                            bnd_angspec[*,npa+1] = angspec
                            ; end get y boundaries correct
                            ; look for phi angles gt 90 and wrap around to -90
                            bnd_pa_red = replicate(0.,ps_size+2)
                            bnd_pa_red[1:npa] = pa_red
                            bnd_pa_red[0] = pa_red - 0.5 ; arbitrary bouns
                            bnd_pa_red[npa+1] = pa_red + 0.5 ; arbitrary bounds
                         endif
                      ;endif else begin
                         ; begin get y boundaries correct
                         bnd_angspec = replicate(!values.f_nan,ns,ps_size+nwrap+2)
                         ;bnd_angspec[*,1:npa] = angspec
                         bnd_angspec[*,1:npa] = angspec[*,0:npa-1]
                         bnd_angspec[*,0] = angspec[*,npa-1]
                         bnd_angspec[*,npa+1] = angspec[*,0]
                         ; end get y boundaries correct
                         ; look for theta angles gt 90 and wrap around to -90
                         bnd_pa_red = replicate(!values.f_nan,ps_size+2)
                         bnd_pa_red[1:npa] = pa_red
                         bnd_pa_red[0] = pa_red[npa-1]-180
                         bnd_pa_red[npa+1] = pa_red[0]+180
                      ;endelse
;                   endelse
                   store_data,an_tname, data= {x:time, y:bnd_angspec ,v:bnd_pa_red}, $
                              dlim={spec:1,zlog:1,ylog:0,datagap:data_gap}
                   if pitch1 gt th_start_angle then th_start_angle = pitch1
                   if pitch2 lt th_end_angle then th_end_angle = pitch2
                   ylim, an_tname, th_start_angle, th_end_angle,log=0,/default
                 endif ; end create tplot vars for theta angle spectra
                 if doangle ne 'none' then begin
                    if size(an_tnames,/type) eq 0 then an_tnames = an_tname $
                       else an_tnames = [an_tnames, an_tname]
                 endif




              endif ; keyword set(units)
;              for i = 0, nmoments-1 do begin
;                  momname = moments_a[i]
;                  value = reform(transpose( struct_value(moms,momname) ) )
;                  tname = prefix + momname
;                  append_array,tplotnames,tname
;;                  printdat,value,varname= comps[i]
;                  store_data,tname,data= { x: moms.time,  y: value }
;                  if size(/n_dimen,value) gt 1 then options,tname,colors='bgr',/def
;              endfor
           endif
        endif ; data exists w/in timerange set by user
    endfor ; loop over data types
endfor ; loop over probes
dprint,dlevel=3,verbose=verbose,'Run Time: ',systime(1)-start,' seconds

tn=tplotnames
options,strfilter(tn,'*_density'),/def ,yrange=[.01,200.],/ystyle,/ylog,ysubtitle='!c[1/cc]'
options,strfilter(tn,'*_velocity'),/def ,yrange=[-800,800.],/ystyle,ysubtitle='!c[km/s]'
options,strfilter(tn,'*_flux'),/def ,yrange=[-1e8,1e8],/ystyle,ysubtitle='!c[#/s/cm2 ??]'
options,strfilter(tn,'*t3'),/def ,yrange=[1,10000.],/ystyle,/ylog,ysubtitle='!c[eV]'

end
