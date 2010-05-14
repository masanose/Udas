;+
; procedure: thm_part_moments
;
; Purpose: Calculates moments and spectra for themis particle distributions.
;
;  For documentation on sun contamination correction keywords that
;  may be passed in through the _extra keyword please see:
;  thm_sst_remove_sunpulse.pro or thm_crib_sst_contamination.pro
;  
; 
;
; Author: Davin Larson 2007
; $Id: $
;-


pro thm_part_moments ,instruments_types=instruments, probes=probes,  $  ;moments=moms,  $
                        moments_types=moments_types, $
                        verbose=verbose, $
                        trange=trange, $
                        tplotnames=tplotnames, $
                        tplotsuffix=tplotsuffix, $
                        set_opts=set_opts, $
                        scpot_suffix=scpot_suffix, mag_suffix=mag_suffix, $    ;inputs: suffix specifying source of magdata and scpot (name - 'th?')
                        comps=comps, get_moments=get_moments,usage=usage, $
                         units=units,_extra=ex

defprobes = '?'
definstruments = 'p??f'
defmoments='density velocity t3 magt3'
start = systime(1)

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

;-----------------------------------------------------------------------

for p = 0,nprobes-1 do begin
    probe= probes_a[p]
    thx = 'th'+probe
    for t=0,ninstruments-1 do begin
        instrument = instruments_a(t)
        format = thx+'_'+instrument
        times= thm_part_dist(format,/times)
;        ns = n_elements(times) * keyword_set(times)
         
        if size(times,/type) ne 5 then begin
          trx = timerange(/current)
           dprint, 'No ',thx,'_',instrument,' data for time range ',trx[0],' to ',trx[1],'. Continuing on to next data type.'
           print, ''
           continue
        endif



        if keyword_set(trange) then  tr = minmax(time_double(trange)) else tr=[1,1e20]
        ind = where(times ge tr[0] and times le tr[1],ns)

        dprint,format,ns,' elements'

        if ns gt 1  then begin
           if keyword_set(mag_suffix) then begin
                dprint,dlevel=2,verbose=verbose,'Interpolating mag data from: ',thx+mag_suffix
                magf = data_cut(thx+mag_suffix,times[ind])
           endif
           if keyword_set(scpot_suffix) then begin
                dprint,dlevel=2,verbose=verbose,'Interpolating sc potential from:',thx+scpot_suffix
                scpot = data_cut(thx+scpot_suffix,times[ind])
           endif
           dat = thm_part_dist(format,index=0,_extra=ex)
           energy = average(dat.energy,2)
           if keyword_set(nmoments) then moms = replicate( moments_3d(), ns )
           time = replicate(!values.d_nan,ns)
           maxnrgs = strmid(instrument,1,1) eq 'e' ? 32 : 16
           dprint,dlevel=3,/phelp,maxnrgs
           spec = replicate(!values.f_nan, ns, maxnrgs )
           energy =  replicate(!values.f_nan,ns, maxnrgs )

           enoise_tot = thm_sst_erange_bin_val(thx,instrument,times,_extra=ex)
           mask_tot = thm_sst_find_masking(thx,instrument,ind,_extra=ex)

           last_angarray = 0
           for i=0L,ns-1  do begin
              dat = thm_part_dist(format,index=ind[i],mask_tot=mask_tot,enoise_tot=enoise_tot,_extra=ex)
;quick fix to time offset problems, jmm, 28-apr-2008
              If(size(dat, /type) Eq 8) Then Begin
                mid_time = (dat.time+dat.end_time)/2.0
                trange2 = [dat.time, dat.end_time]                ; quick fix for now, I am currently working on a better solution.  DL
                dat.time = mid_time
                str_element, dat, 'end_time', /delete
                str_element, dat, 'trange', trange2, /add_replace
              Endif
              angarr=[dat.theta,dat.phi,dat.dtheta,dat.dphi]
              if array_equal(last_angarray,angarr) eq 0 then begin   ; Sense change in mode
                 last_angarray = angarr
                 domega = 0
              printdat,angarr
                 dprint,dlevel=2,verbose=verbose,'Index=',i,' New mode at: ',time_string(dat.time)
              endif
              time[i] = dat.time
              dim = size(/dimen,dat.data)
              dim_spec = size(/dimensions,spec)

              if keyword_set(units) then begin
                 udat = conv_units(dat,units+'')
                 bins = udat.bins
                 dim_data = size(/dimensions,udat.data)
                 nd = size(/n_dimensions,udat.data)
                 dim_bins = size(/dimensions,bins)
                 if array_equal(dim_data,dim_bins) eq 0 then bins = replicate(1,udat.nenergy) # bins   ; special case for McFadden's 3D structures
                 sp = nd eq 1 ? udat.data   : total(udat.data * (bins ne 0),2) / total(bins ne 0,2)
                 en = nd eq 1 ? udat.energy : average(udat.energy,2)
                 spec[ i, 0:dim[0]-1] = sp
                 energy[ i,0:dim[0]-1] = en
              endif
              if  nmoments ne 0 then begin
;                  dat.dead = 0
                  if keyword_set(magf) then  dat.magf=magf[i,*]
                  if keyword_set(scpot) then dat.sc_pot=scpot[i]
                  moms[i] = moments_3d( dat , domega=domega )
              endif
              dprint,dwait=10.,format,i,'/',ns;,'    ',time_string(dat.time)   ; update every 10 seconds
           endfor

           if not keyword_set(no_tplot) then begin
              prefix = thx+'_'+instrument+'_'
              suffix = '_'+strlowcase(units)+tplotsuffix
              if keyword_set(units) then begin
                 tname = prefix+'en'+suffix
                 energyval = float( average(energy,1))
                 kev = energyval gt 1000.
                 ylabels = string(energyval/(1000^kev),format='(f4.0)')+([' eV',' keV'])[kev]
                 store_data,tname, data= {x:time, y:spec ,v:energy },dlim={spec:1,zlog:1,ylog:1 $
                   ,labels:ylabels,labflag:-1}
                 append_array,tplotnames,tname
              endif
              for i = 0, nmoments-1 do begin
                  momname = moments_a[i]
;                  value = reform(transpose( struct_value(moms,momname) ) )
;fix for transpose bombing when there is only  1 element, jmm, 13-feb-2008
                  value = struct_value(moms, momname)
                  if(n_elements(value) gt 1) then value = reform(transpose(temporary(value)))
                  tname = prefix + momname + tplotsuffix
                  append_array,tplotnames,tname
;                  printdat,value,varname= comps[i]
                  store_data,tname,data= { x: moms.time,  y: value }
                  if size(/n_dimen,value) gt 1 then options,tname,colors='bgr',/def
              endfor
           endif
        endif
    endfor
endfor
dprint,dlevel=3,verbose=verbose,'Run Time: ',systime(1)-start,' seconds

tn=tplotnames
options,strfilter(tn,'*_density'),/def ,yrange=[.01,200.],/ystyle,/ylog,ysubtitle='!c[1/cc]'
options,strfilter(tn,'*_velocity'),/def ,yrange=[-800,800.],/ystyle,ysubtitle='!c[km/s]'
options,strfilter(tn,'*_flux'),/def ,yrange=[-1e8,1e8],/ystyle,ysubtitle='!c[#/s/cm2 ??]'
options,strfilter(tn,'*t3'),/def ,yrange=[1,10000.],/ystyle,/ylog,ysubtitle='!c[eV]'

end


