;+
; PROCEDURE: ERG_LOAD_GMAG_NIPR
;   erg_load_gmag_nipr, site = site, $
;                     datatype=datatype, $
;                     trange=trange, $
;                     verbose=verbose, $
;                     downloadonly=downloadonly, $
;                     no_download=no_download
;
; Purpose:
;   Loading the fluxgate magnetometer data obtained by NIPR.
;
; Note:
;   At the moment, CDF files are avaible only for Syowa data (syo) 
;   with 1-sec resolution in the interval from Feb. 4, 2003 to 
;   Jan. 31, 2010.
;
; KEYWORDS:
;   site  = Observatory name, example, erg_load_gmag_nipr, site='syo',
;           the default is 'all', i.e., load all available stations.
;           This can be an array of strings, e.g., ['syo', 'hus']
;           or a single string delimited by spaces, e.g., 'syo hus'.
;           Available sites: syo hus tjo aed isa
;   datatype = Time resolution. Please notice that '1sec' means nearly
;           1-sec time resolution. Even if datatype was set to '1sec', 
;           the time resolution corresponds to
;           2sec : syo(1981-1997), hus & tjo(1984-2001/08), isa(1984-1989), 
;                  aed(1989-1999/10)
;           1sec : syo(1997-present)
;           0.5sec  : hus & tjo(2001/09-present), aed(2001/09-2008/08)
;           Available datatype: 1sec(default) or 20hz
;   trange = (Optional) Time range of interest  (2 element array).
;   /verbose: set to output some useful info
;   /downloadonly: if set, then only download the data, do not load it 
;           into variables.
;   /no_download: use only files which are online locally.
;
; EXAMPLE:
;   erg_load_gmag_nipr, site='syo', $
;                 trange=['2003-11-20/00:00:00','2003-11-21/00:00:00']
;
; Written by H. Tadokoro, June 1, 2010
; Revised by Y.-M. Tanaka, December 6, 2010 (ytanaka at nipr.ac.jp)
; The prototype of this procedure was written by Y. Miyashita, Apr 22, 2010, 
;        ERG-Science Center, STEL, Nagoya Univ.
;-

pro erg_load_gmag_nipr, site=site, datatype=datatype, $
        trange=trange, verbose=verbose, downloadonly=downloadonly, $
	no_download=no_download

;*************************
;***** Keyword check *****
;*************************
;--- verbose
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
  '20hz': varformat='hdz_20hz'
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
  if keyword_set(no_download) then source.no_download = 1

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

  endif

  ;--- Load data into tplot variables
  if(not keyword_set(downloadonly)) then downloadonly=0

  if(downloadonly eq 0) then begin
    prefix_tmp='nipr_'
    cdf2tplot, file=files, verbose=source.verbose, prefix=prefix_tmp, $
	varformat=varformat

    tplot_name_tmp=prefix_tmp+varformat
    len=strlen(tnames(tplot_name_tmp))

    if  len eq 0 then begin
      ;--- Quit if no data have been loaded
      print, 'No tplot var loaded for '+site_code[i]+'.'
    endif else begin
      ;--- Rename tplot variables
      tplot_name_new='iug_mag_'+site_code[i]
      copy_data, tplot_name_tmp, tplot_name_new
      store_data, tplot_name_tmp, /delete

      ;--- Missing data -1.e+31 --> NaN
      tclip, tplot_name_new, -1e+5, 1e+5, /overwrite

      ;--- Labels
      options, tplot_name_new, labels=['H','D','Z'], $
                         ytitle = strupcase(strmid(site_code[i],0,3)), $
                         ysubtitle = '[nT]'
    endelse
  endif

endfor

;---
return
end

