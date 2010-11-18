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
; revised by Y. Tanaka, November 19, 2010
;-

pro iug_load_gmag_nipr, site = site, datatype=datatype, $
        trange=trange, verbose=verbose, downloadonly=downloadonly


;*************************
;***** Keyword check *****
;*************************
; verbose
if ~keyword_set(verbose) then verbose=0

;--- all sites (default)
site_code_all = strsplit('syo hus tjo aed isa', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if site_code[0] eq '' then return

print, site_code

;--- validate datatype
if(not keyword_set(datatype)) then datatype='1sec'

datatype_all=strsplit('1sec 20hz', /extract)
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,datatype_all, $
                                /ignore_case, /include_all)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse

datatype=datatype[0]

case strlowcase(datatype) of
  '1sec': varformat='hdz_sec'
  '20hz':   varformat='hdz_20hz'
  else:   varformat='hdz_sec'
endcase

instr='fmag'

;*************************************************************************
;***** Download files, read data, and create tplot vars at each site *****
;*************************************************************************
;=================================
;=== Loop on downloading files ===
;=================================
;*** load CDF ***
for i=0,n_elements(site_code)-1 do begin
  
  ;--- Create (and initialize) a data file structure 
  source = file_retrieve(/struct)
  source.verbose = verbose

  ;--- Set parameters for the data file class
  source.local_data_dir  = root_data_dir() + 'iugonet/nipr/'
  source.remote_data_dir = 'http://www.nipr.ac.jp/~ytanaka/data/'

  relpathnames1 = file_dailynames(file_format='YYYY', trange=trange)
  relpathnames2 = file_dailynames(file_format='YYYYMMDD', trange=trange)
  relpathnames  = instr+'/'+site_code[i]+'/'+strlowcase(datatype)+'/'+$
    relpathnames1 + '/nipr_????_'+instr+'_'+site_code[i]+'_'+$
    relpathnames2 + '_v??.cdf'

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
    print, 'PI: ', gatt.PI_name
    print, 'Affiliations: ', gatt.PI_affiliation
    print, ''
    print, 'Rules of the Road for NIPR Fluxgate Magnetometer Data:'
    print, gatt.text
    print, ''
    print, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
    print, '**************************************************************************************'

    ;--- Load data into tplot variables
    if(not keyword_set(downloadonly)) then downloadonly=0

    if(downloadonly eq 0) then begin
      cdf2tplot, file=files, verbose=source.verbose, varformat=varformat

      ;--- Rename tplot variables
      tplot_name='iug_mag_'+site_code[i]
      copy_data, varformat, tplot_name
      store_data, varformat, /delete

      ;--- Missing data -1.e+31 --> NaN
      tclip, newvar, -1e+5, 1e+5, /overwrite

      ;--- Labels
      options, tplot_name, labels=['H','D','Z'], $
                           ytitle = strupcase(strmid(site_code[i],0,3)), $
                           ysubtitle = '[nT]'
    endif
  endif
endfor

;---
return
end

