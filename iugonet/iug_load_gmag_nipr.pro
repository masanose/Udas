;+
; PROCEDURE: iug_load_gmag_nipr
;   to load the 1-sec resolution geomagnetic data from the NIPR site.
;
; KEYWORDS:
;   site  = Observatory name, example, iug_load_gmag_nipr, site='syo',
;           the default is 'all', i.e., load all available stations.
;           This can be an array of strings, e.g., ['syo', 'hus']
;           or a single string delimited by spaces, e.g., 'syo hus'.
;           Sites:  syo
;   datatype = Time resolution. '1sec' for 1 sec. (Not available.)
;   /downloadonly, if set, then only download the data, do not load it into variables.
;   trange = (Optional) Time range of interest  (2 element array).
;
; EXAMPLE:
;   iug_load_gmag_nipr, site='syo', $
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
;   $LastChangedDate: 2010-05-17 12:53:39 +0900 (Mon, 17 May 2010) $
;   $LastChangedRevision: 11 $
;   $URL: $
;
; revised by H. Tadokoro, June 1, 2010
; revised by Y. Tanaka, August 11, 2010
;-

;pro erg_load_gmag_mm210, site=site, datatype=datatype, $
;        downloadonly=downloadonly, trange=trange
pro iug_load_gmag_nipr, site = site, datatype=datatype, $
        downloadonly=downloadonly, trange=trange, verbose=verbose


;*************************
;***** Keyword check *****
;*************************
; verbose
if ~keyword_set(verbose) then verbose=0

; site
; list of sites
vsnames = 'syo'
vsnames_all = strsplit(vsnames, ' ', /extract)

; validate sites
if(keyword_set(site)) then site_in = site else site_in = 'syo'
mag_sites = thm_check_valid_name(site_in, vsnames_all, $
                                /ignore_case, /include_all, /no_warning)
if mag_sites[0] eq '' then return

; number of valid sites
nsites = n_elements(mag_sites)
print,nsites

;*** time resolution ***
if(not keyword_set(datatype)) then datatype='1sec'
case strlowcase(datatype) of
  '1sec': varformat='*hdz_1sec*'
  '1min': varformat='*hdz_1min*'
  '1h':   varformat='*hdz_1h*'
  else:   varformat='*hdz_1sec*'

endcase

;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
; make remote path, local path, and download files
for i=0, nsites-1 do begin

  now_site = strlowcase(strmid(mag_sites[i],0,3))
  print,now_site

  ;*** load CDF ***

  ;--- Create (and initialize) a data file structure 
  source = file_retrieve(/struct)
  source.verbose = verbose

  ;--- Set parameters for the data file class 
  source.local_data_dir  = root_data_dir() + 'iugonet/nipr/gmag/'+now_site
  source.remote_data_dir = '/pl09/iugonet/data/cdf/fmag/syo/'

  ;--- Set the file path which is added to source.local_data_dir/remote_data_dir.
 
  ;--- Generate the file paths by expanding wilecards of date/time 
  ;    (e.g., YYYY, YYYYMMDD) for the time interval set by "timespan"
  ;relpathnames = file_dailynames(file_format=pathformat) 

  relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
  relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange) 
  relpathnames3 = file_dailynames(file_format='MM', trange=trange) 
  relpathnames4 = file_dailynames(file_format='DD', trange=trange) 

   relpathnames  = relpathnames1 $
                 + '/nipr_1sec_fmag_'+now_site+'_' + relpathnames2 + '_v01.cdf'


  ;--- Download the designated data files from the remote data server
  ;    if the local data files are older or do not exist. 
  files = file_retrieve(relpathnames, _extra=source, /last_version)

  ;--- print PI info and rules of the road
  if((findfile(files[0]))[0] ne '') then begin
    gatt = cdf_var_atts(files[0])

    print, '**************************************************************************************'
    print, gatt.project
    print, gatt.Logical_source_description
    print, ''
  ;  print, 'Information about ', gatt.Station_code
;    print, 'PI and Host PI(s): ', gatt.PI_name
;    print, 'Affiliations: ', gatt.PI_affiliation
    print, ''
;    print, 'Rules of the Road for 210 MM Data Use:'
    print, 'Rules of the Road for syowa Data Use:'
    print, gatt.text
;    print, ''
    print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
    print, '**************************************************************************************'

    ;--- Load data into tplot variables
    if(not keyword_set(downloadonly)) then downloadonly=0

    if(downloadonly eq 0) then begin
      cdf2tplot, file=files, verbose=source.verbose, $
        prefix='nipr_', suffix='_'+mag_sites[i], varformat=varformat

      ;--- Missing data -1.e+31 --> NaN
      tclip, 'nipr_hdz_1*_???', -1e+4, 1e+4, /overwrite
      tclip, 'nipr_hdz_'+strlowcase(datatype)+'_'+mag_sites[i], -1e+4, 1e+4, /overwrite

      ;--- Labels
;      options, 'mm210_hdz_'+strlowcase(datatype)+'_'+site_code[i], labels=['H','D','Z']
      options, 'nipr_hdz_'+strlowcase(datatype)+'_'+mag_sites[i], labels=['H','D','Z']
    endif
  endif
endfor

return
end
