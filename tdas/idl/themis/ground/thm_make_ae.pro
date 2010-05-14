;+
;	Name:       THM_MAKE_AE
;
;	Purpose:    This routine calculates the "pseudo" AE, AL, and AU geomagnetic
;                   indices by using ground magnetometer data from THEMIS GBOs. The names
;                   of all stations used for calculation are printed on the screen.
;                   In future, it will be possible to include ground data from other
;                   magnetometer networks. Note that currently the calculation of
;                   the "pseudo" indices does not subtract quiet day variation but
;                   simply the median.
;
;       Syntax:     THM_MAKE_AE [, RES = float] [, SITES = string ] [, /NO_LOAD ]
;
;	Parameters: None.
;
;	Keywords:   res     = sampling interval (by default 60 sec)
;                   sites   = observatory name; default is to use high-latitude
;                             THEMIS sites: 'atha chbg ekat fsim fsmi fykn
;                                            gako gbay gill inuv kapu
;                                            kian kuuj mcgr pgeo pina
;                                            rank snap snkq tpas whit
;                                            yknf'
;                             If set to 'all', all available sites
;                             will be loaded and used.
;                   no_load = if set, use existing gmag (THEMIS) tplot variables which have
;                             already been loaded into the active TDAS environment
;                             if not set, load gmag data (either remotely or from computer disk)
;
;	Example:    see crib sheet "thm_crib_AE.pro"
;
;       Notes:      Written by Andreas Keiling, 15 May 2008
;
;       Modifications:
;                   Edited header, put subroutine before main body so
;                   that it would compile before being called by the
;                   main body, W.M.Feuerstein, 6/2/2008.
;                   Changed default sites to be only high-lat THEMIS
;                   stations:  jmm, 25-nov-2009
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2009-11-30 21:42:00 -0800 (Mon, 30 Nov 2009) $
; $LastChangedRevision: 6961 $
; $URL $
;-


pro tdespike_AEALAU, lower, upper, quant=quant
; this routine removes artificial spikes
;
; variables:  lower = lower cutoff of spikes to be removed
;             upper = upper cutoff of spikes to be removed
;

get_data, quant, data=A_index
last=n_elements(A_index.y)-1
NaN=!values.f_nan

indices = where(A_index.y gt upper OR A_index.y lt lower , count)
if count ne 0 then begin
   for k=0,count-1 do begin
       i=indices[k]
       if (i eq 0 OR i eq 1 ) then begin
          A_index.y[0]=NaN
          A_index.y[1]=NaN
       endif else begin
          if (i eq last-1 OR i eq last) then begin
               A_index.y[last-1]=NaN
               A_index.y[last] = NaN
          endif else begin
               A_index.y[i-2]=NaN
               A_index.y[i-1]=NaN
               A_index.y[i] = NaN
               A_index.y[i+1]=NaN
               A_index.y[i+2]=NaN
          endelse
       endelse
   endfor
endif

store_data, quant+'_despike', data=A_index

end

;#######################################################

pro thm_make_AE, res = res, sites = sites, no_load = no_load, _extra = _extra


; set default time resolution
;----------------------------
if not keyword_set(res) then res = 60d   ; 60 sec resolution
res=double(res)


; load gmag data
;----------------
if not keyword_set(no_load) then begin
;allow for sites keyword to operate
  if keyword_set(sites) then begin
    thm_load_gmag, site = vsites, /valid_names ;check name validity here:
    x4 = where(strlen(vsites) Eq 4) ;avoid alt greenland sites
    vsites = vsites[x4]
    site_load = thm_check_valid_name(sites, vsites, /ignore_case, /include_all, /no_warning)
    If(is_string(site_load) Eq 0) Then Begin
      message, /info, 'No Valid sites? '+sites
      Return
    Endif
  endif else begin
    site_load = ['atha', 'chbg', 'ekat', 'fsim', 'fsmi', 'fykn', $
                 'gako', 'gbay', 'gill', 'inuv', 'kapu', 'kian', $
                 'kuuj', 'mcgr', 'pgeo', 'pina', 'rank', 'snap', $
                 'snkq', 'tpas', 'whit', 'yknf'] ;made an array to facilitate the use of split_vec later
  endelse
  thm_load_gmag, /subtract_median, site = site_load
endif
sites_varnames = tnames('thg_mag_'+site_load)
If(is_string(sites_varnames) Eq 0) Then Begin
  message, /info, 'No Valid sites? '
  Return
Endif
split_vec, sites_varnames
stations = sites_varnames+'_x'


; calculate indices
;------------------
superpo_histo,stations,dif='thmAE',min='thmAL',max='thmAU',res=res


; thresholds can be changed if desired
;-------------------------------------
cut=1500
tdespike_AEALAU,-cut,cut, quant='thmAE'
tdespike_AEALAU,-cut,cut, quant='thmAL'
tdespike_AEALAU,-cut,cut, quant='thmAU'

clean_spikes, 'thmAE_despike', new_name = 'thmAE', thresh = 5
clean_spikes, 'thmAL_despike', new_name = 'thmAL', thresh = 5
clean_spikes, 'thmAU_despike', new_name = 'thmAU', thresh = 5

options,'thmAE',ytitle='THEMIS!CAE Index'
options,'thmAL',ytitle='THEMIS!CAL Index'
options,'thmAU',ytitle='THEMIS!CAU Index'


; delete obsolete data
;---------------------
del_data,'thmAE_despike'
del_data,'thmAU_despike'
del_data,'thmAL_despike'


; print station names used for calculation
;-----------------------------------------
print,'The following stations contributed to the index calculation:'
tplot_names, stations           ;jmm, 2009-11-30
print,'-----------------------------'

end



