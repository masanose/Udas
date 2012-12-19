;+
; PROCEDURE: iug_load_lfrto
;
; PURPOSE:
;   To load the Low Frequency Radio Transmitter Observation data from the Tohoku University site 
;
; KEYWORDS:
;   site  = Observatory name, example, iug_load_lfrto, site='ath',
;           the default is 'ath', athabasca station.
;           This can be an array of strings, e.g., ['ath', 'nal']
;           or a single string delimited by spaces, e.g., 'ath nal'.
;           Sites:  ath nal
;   trans = Transmitter code, example, iug_load_lfrto, trans='wwvb',
;           the default is 'all', i.e., load all available transmitter.
;           This can be an array of strings, e.g., ['wwvb', 'ndk']
;           or a single string delimited by spaces, e.g., 'wwvb ndk'.
;           Transmitter:  wwvb ndk nlk npm nau nrk nwc msf dcf
;   datatype = Time resolution. '30sec' for 30 sec.
;              The default is '30sec'.
;   /downloadonly, if set, then only download the data, do not load it into variables.
;   /no_server, use only files which are online locally.
;   /no_download, use only files which are online locally. (Identical to no_server keyword.)
;   trange = (Optional) Time range of interest  (2 element array).
;
; EXAMPLE:
;   iug_load_lfrto, site='ath', datatype='30sec', $
;                        trange=['2010-10-24/00:00:00','2010-10-25/00:00:00']
;
; NOTE: See the rules of the road.
;       For more information, see http://iprt.gp.tohoku.ac.jp/
;
; Written by: M. Yagi, Oct 2, 2012
;             PPARC, Tohoku Univ.
;
;   $LastChangedBy: M.Yagi $
;   $LastChangedDate: 2012-12-18 $
;   $URL:
;-
pro iug_load_lfrto, site=site, trans=trans, parameter=parameter, datatype=datatype,$
         downloadonly=downloadonly, no_server=no_server, no_download=no_download,$
         trange=trange

;--- reciever (site)
site_code_all = strsplit('ath nal', /extract)
if(n_elements(site) eq 0) then site='ath'
site_code=thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)
if(site_code[0] eq '') then return
print, site_code


;--- transmitter (trans)
trans_code_ath_all = strsplit('wwvb ndk nlk npm nau nrk nwc', /extract)
trans_code_nal_all = strsplit('msf dcf nrk', /extract)
trans_code_all = strsplit('wwvb ndk nlk npm nau nrk nwc msf dcf nrk', /extract)
if(n_elements(trans) eq 0) then trans='all'
trans_code_ath = thm_check_valid_name(trans, trans_code_ath_all, /ignore_case, /include_all, /no_warning)
trans_code_nal = thm_check_valid_name(trans, trans_code_nal_all, /ignore_case, /include_all, /no_warning)
if(trans_code_ath[0] eq '' and trans_code_nal[0] eq '') then begin
  dprint,'!!! Trans code error !!!'
;  dprint,'Please choose valid trans code as follows:'
;  dprint,'all : wwvb ndk nlk npm nau nrk nwc msf dcf,'
;  dprint,'ath : wwvb ndk nlk npm nau nrk nwc,'
;  dprint,'nal : msf dcf nrk.'
  return
endif


;--- time resolution (datatype)
tres_all = ['30sec']
if(n_elements(datatype) eq 0) then datatype='all'
tres = thm_check_valid_name(datatype, tres_all, /ignore_case, /include_all)
if (tres[0] eq '') then return


;--- power & phase (parameter)
param_all = strsplit('power phase', /extract)
if(n_elements(parameter) eq 0) then parameter='all'
param = thm_check_valid_name(parameter, param_all, /ignore_case, /include_all)


;--- Download options
if keyword_set(downloadonly) then source.downloadonly=1
if keyword_set(no_server)    then source.no_server=1
if keyword_set(no_download)  then source.no_download=1

if(not keyword_set(downloadonly)) then downloadonly=0
if(not keyword_set(no_server))    then no_server=0
if(not keyword_set(no_download))  then no_download=0


;--- data file structure
source = file_retrieve(/struct)
filedate  = file_dailynames(file_format='YYYYMMDD')

for i=0,n_elements(site_code)-1 do begin
  ;--- Set the file path
  source.local_data_dir = root_data_dir() + 'iugonet/TohokuU/radio_obs/'+site_code[i]+'/lf/'
  source.remote_data_dir = 'http://iprt.gp.tohoku.ac.jp/lf/cdf/'+site_code[i]+'/'
  case site_code[i] of
    'ath': trans_code=trans_code_ath
    'nal': trans_code=trans_code_nal
  endcase

  for j=0,n_elements(trans_code)-1 do begin
    for k=0,n_elements(tres)-1 do begin

      ;--- Download file
      relfnames = 'lfrto'+'_'+tres[k]+'_'+site_code[i]+'_'+trans_code[j]+'_'+filedate+'_v01.cdf'
      datfiles  = file_retrieve(relfnames, _extra=source)

      ;--- Load data into tplot variables
      if(file_test(datfiles) eq 1 and downloadonly eq 0) then begin
        for l=0,n_elements(param)-1 do begin
          cdf2tplot, file=source.local_data_dir+relfnames,varformat='lf_'+param[l]+'_'+tres[k]

          ;--- Rename
          copy_data,  'lf_'+param[l]+'_'+tres[k],'iug_lf_'+site_code[i]+'_'+trans_code[j]+'_'+param[l]+'_'+tres[k]
          store_data, 'lf_'+param[l]+'_'+tres[k], /delete
        endfor
      endif

    endfor
  endfor

endfor


;--- Acknowledgement
datfile = source.local_data_dir+relfnames[0]
if (file_test(datfile) eq 1) then begin
  gatt = cdf_var_atts(datfile)
  dprint, '**********************************************************************'
  dprint, gatt.project
  dprint, ''
 ;dprint, 'Information about ', gatt.Receiver_station_code
  dprint, 'PI and Host PI(s): ', gatt.PI_name
 ;dprint, 'Affiliations: ', gatt.PI_affiliation
  dprint, 'Affiliations: ', 'PPARC, Tohoku University'
  dprint, ''
  dprint, 'Rules of the Road for LFRTO Data Use:'
  for igatt=0, n_elements(gatt.text)-1 do print_str_maxlet, gatt.text[igatt], 70
  dprint, ''
  dprint, gatt.LINK_TEXT, ' ', gatt.HTTP_LINK
  dprint, '**********************************************************************'
  dprint, ''
endif

return
end
