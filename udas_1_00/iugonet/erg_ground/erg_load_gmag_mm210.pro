;+
; PROCEDURE: erg_load_gmag_mm210
;   to load the 210 MM geomagnetic data from the STEL ERG-SC site 
;
; KEYWORDS:
;   site  = Observatory name, example, erg_load_gmag_mm210, site='rik',
;           the default is 'all', i.e., load all available stations.
;           This can be an array of strings, e.g., ['rik', 'onw']
;           or a single string delimited by spaces, e.g., 'rik onw'.
;           Sites:  tik zgn yak irt ppi bji lnp mut ptn wtk
;                   lmt kat ktn chd zyk mgd ptk msr rik onw
;                   kag ymk cbi gua yap kor ktb bik wew daw
;                   wep bsv dal can adl kot cst ewa asa mcq
;   datatype = Time resolution. '1min' for 1 min, and '1h' for 1 h.
;              The default is '1min'.
;   /downloadonly, if set, then only download the data, do not load it into variables.
;   trange = (Optional) Time range of interest  (2 element array).
;
; EXAMPLE:
;   erg_load_gmag_mm210, site='rik onw', datatype='1min', $
;                        trange=['2003-11-20/00:00:00','2003-11-21/00:00:00']
;
; NOTE: See the rules of the road.
;       For more information, see http://stdb2.stelab.nagoya-u.ac.jp/mm210/
;
; Written by: Y. Miyashita, Apr 22, 2010
;             ERG-Science Center, STEL, Nagoya Univ.
;             erg-sc-core at st4a.stelab.nagoya-u.ac.jp
;
;   $LastChangedBy: miyasita $
;   $LastChangedDate: 2010-07-30 11:53:38 +0900 (Fri, 30 Jul 2010) $
;   $LastChangedRevision: 44 $
;   $URL: file:///var/svn/repos/ergsc/branches/test_release_201007/erg/ground/geomag/erg_load_gmag_mm210.pro $
;-

pro erg_load_gmag_mm210, site=site, datatype=datatype, $
        downloadonly=downloadonly, trange=trange

;*** site codes ***
;--- aliases
if(keyword_set(site)) then begin
  site=strsplit(strlowcase(site), ' ', /extract)
  if(where(site eq 'tix') ne -1) then site[where(site eq 'tix')]='tik'
  if(where(site eq 'lem') ne -1) then site[where(site eq 'lem')]='lmt'
  if(where(site eq 'gam') ne -1) then site[where(site eq 'gam')]='gua'
  if(where(site eq 'wwk') ne -1) then site[where(site eq 'wwk')]='wew'
  if(where(site eq 'drw') ne -1) then site[where(site eq 'drw')]='daw'
  if(where(site eq 'brv') ne -1) then site[where(site eq 'brv')]='bsv'
  if(where(site eq 'dlb') ne -1) then site[where(site eq 'dlb')]='dal'
endif

;--- all sites (default)
site_code_all = strsplit( $
   'tik zgn yak irt ppi bji lnp mut ptn wtk ' $
  +'lmt kat ktn chd zyk mgd ptk msr rik onw ' $
  +'kag ymk cbi gua yap kor ktb bik wew daw ' $
  +'wep bsv dal can adl kot cst ewa asa mcq', $
  ' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;*** time resolution ***
if(not keyword_set(datatype)) then datatype='1min'

case strlowcase(datatype) of
  '1min': varformat='*hdz_1min*'
  '1h':   varformat='*hdz_1h*'
  else:   varformat='*hdz_1min*'
endcase

fres='1min'

;*** load CDF ***
for i=0,n_elements(site_code)-1 do begin

  ;--- Create (and initialize) a data file structure 
  source = file_retrieve(/struct)

  ;--- Set parameters for the data file class 
  source.local_data_dir  = root_data_dir() + 'ergsc/'
  source.remote_data_dir = 'http://gemsissc.stelab.nagoya-u.ac.jp/data/ergsc/'

  ;--- Set the file path which is added to source.local_data_dir/remote_data_dir.
  ;pathformat = 'ground/geomag/mm210/'+fres+'/SSS/YYYY/mm210_'+fres+'_SSS_YYYYMMDD_v??.cdf'

  ;--- Generate the file paths by expanding wilecards of date/time 
  ;    (e.g., YYYY, YYYYMMDD) for the time interval set by "timespan"
  ;relpathnames = file_dailynames(file_format=pathformat) 

  relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
  relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange) 
  relpathnames  = 'ground/geomag/mm210/'+fres+'/'+site_code[i]+'/'+relpathnames1 $
                + '/mm210_'+fres+'_'+site_code[i]+'_'+relpathnames2+'_v??.cdf'
  ;print,relpathnames

  ;--- Download the designated data files from the remote data server
  ;    if the local data files are older or do not exist. 
  files = file_retrieve(relpathnames, _extra=source, /last_version)

  ;--- print PI info and rules of the road
  if(file_test(files[0])) then begin
    gatt = cdf_var_atts(files[0])

    print, '**************************************************************************************'
    ;print, gatt.project
    print, gatt.Logical_source_description
    print, ''
    print, 'Information about ', gatt.Station_code
    print, 'PI and Host PI(s): ', gatt.PI_name
    print, 'Affiliations: ', gatt.PI_affiliation
    print, ''
    print, 'Rules of the Road for 210 MM Data Use:'
    print, gatt.text
    print, ''
    print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
    print, '**************************************************************************************'

    ;--- Load data into tplot variables
    if(not keyword_set(downloadonly)) then downloadonly=0

    if(downloadonly eq 0) then begin
      cdf2tplot, file=files, verbose=source.verbose, $
                 prefix='mm210_', suffix='_'+site_code[i], varformat=varformat

      ;--- Missing data -1.e+31 --> NaN
      tclip, 'mm210_hdz_1*_???', -1e+4, 1e+4, /overwrite
      tclip, 'mm210_hdz_'+strlowcase(datatype)+'_'+site_code[i], -1e+4, 1e+4, /overwrite

      ;--- Labels
;      options, 'mm210_hdz_'+strlowcase(datatype)+'_'+site_code[i], labels=['H','D','Z']
      options, 'mm210_hdz_'+strlowcase(datatype)+'_'+site_code[i], labels=['Ch1','Ch2','Ch3']
    endif
  endif
endfor

;---
return
end
