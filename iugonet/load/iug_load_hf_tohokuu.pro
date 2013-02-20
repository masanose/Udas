;+
; PROCEDURE: iug_load_hf_tohokuu
;
; PURPOSE:
;   To load the Jupiter's/solar wide band spectral data in HF-band 
;
; KEYWORDS:
;   site  = Observatory name. Only 'iit' is allowed.
;   parameter = The polarization of the radiowave.
;               'RH' for right handed, and 'LH' for left handed.
;   /downloadonly, if set, then only download the data, do not load it into variables.
;   /no_download: use only files which are online locally.
;   trange = (Optional) Time range of interest  (2 element array).
;
; EXAMPLE:
;   iug_load_lfrto, site='ath', datatype='30sec', $
;                        trange=['2010-10-24/00:00:00','2010-10-25/00:00:00']
;
; NOTE: See the rules of the road.
;       For more information, see http://iprt.gp.tohoku.ac.jp/
;
; Written by: M.Yagi, Oct 2, 2012
;             PPARC, Tohoku Univ.
;
;   $LastChangedBy: M.Yagi $
;   $LastChangedDate: 2013-01-09 $
;   $URL:
;-

pro iug_load_hf_tohokuU, site=site, parameter=parameter,$
         downloadonly=downloadonly, no_download=no_download,$
         trange=trange

;--- site
site_code_all = strsplit('iit', /extract)
if(n_elements(site) eq 0) then site='iit'
site_code=thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if(site_code[0] eq '') then return
print, site_code

;--- parameter
param_all = strsplit('RH LH', /extract)
if(n_elements(parameter) eq 0) then parameter='all'
param = thm_check_valid_name(parameter, param_all, /ignore_case, /include_all)
if(param[0] eq '') then return
print, param

;--- data file structure
source = file_retrieve(/struct)
filedate  = file_dailynames(file_format='YYYYMMDD',trange=trange)

;--- Download options
if keyword_set(downloadonly) then source.downloadonly=1
if keyword_set(no_download)  then source.no_download=1
if(not keyword_set(downloadonly)) then downloadonly=0

for i=0,n_elements(site_code)-1 do begin
  ;--- Set the file path
  source.local_data_dir = root_data_dir() + 'iugonet/TohokuU/radio_obs/iit/hfspec/'
  source.remote_data_dir = 'http://ariel.gp.tohoku.ac.jp/~jupiter/it_hf/cdf/'

  ;--- Download file
  relfnames = 'it_h1_hf_'+filedate+'_v01.cdf'
  datfiles  = file_retrieve(relfnames, _extra=source)

  ;--- Load data into tplot variables
  if(file_test(datfiles) eq 1 and downloadonly eq 0) then begin
    for j=0,n_elements(param)-1 do begin

      print,param
      cdf2tplot, file=datfiles,varformat=strupcase(param[j])
      ;--- Rename
      copy_data,  strupcase(param[j]),'iug_iit_hf_'+param[j]
      store_data, strupcase(param[j]), /delete
    endfor
  endif
endfor


;--- Acknowledgement
datfile = source.local_data_dir+relfnames[0]
if (file_test(datfile) eq 1) then begin
  gatt = cdf_var_atts(datfile)
  dprint, ''
  dprint, '**********************************************************************'
  dprint, gatt.project
  dprint, ''
  dprint, 'PI and Host PI(s): ', 'Atsushi Kumamoto'
  dprint, 'Affiliations: ', 'PPARC, Tohoku University'
  dprint, ''
  dprint, 'Rules of the Road for HF Data Use:'
  dprint, 'When the data is used in or contributes to a presentation or publication, you should let us know and make acknowledgement to the Planetary Plasma and Atmospheric Research Center, Tohoku University.'
  dprint, '**********************************************************************'
  dprint, ''
endif

return
end
