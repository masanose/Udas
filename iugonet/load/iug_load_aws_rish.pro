;+
;
;NAME:
;iug_load_aws_rish
;
;PURPOSE:
;  Queries the RISH server for the surface meterology data taken by the automatic weather 
;  station (AWS) and loads data into tplot format.
;
;SYNTAX:
; iug_load_aws_rish, datatype = datatype, site=site, $
;                    downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_load_aws_rish, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  site = AWS observation site.  
;         For example, iug_load_aws_rish, site = 'bik'.
;         The default is 'all', i.e., load all available observation points.
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;CODE:
;  A. Shinbori, 28/02/2013.
;  
;MODIFICATIONS:
;
;   
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

  
pro iug_load_aws_rish, datatype = datatype, $
   site = site, $
   trange = trange, $
   verbose = verbose, $
   downloadonly=downloadonly

;**********************
;Verbose keyword check:
;**********************
if (not keyword_set(verbose)) then verbose=2


;***************
;Datatype check:
;***************
if (not keyword_set(datatype)) then datatype= 'troposphere'


;****************
;Site code check:
;****************
;--- all sites (default)
site_code_all = strsplit('bik ktb mnd pon sgk',' ', /extract)

;--- check site codes
if (not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

if n_elements(site_code) eq 1 then begin
   if site_code eq '' then begin
      print, 'This station code is not valid. Please input the allowed keywords, all, bik, ktb, mnd, pon, and sgk.'
      return
   endif
endif

print, site_code


;===============================
;======Load data of AWS=========
;===============================
for i=0, n_elements(site_code)-1 do begin
  ;load of aws data at the Indonesian sites
   if (site_code[i] eq 'bik') or (site_code[i] eq 'mnd') or (site_code[i] eq 'pon') then begin
      iug_load_aws_id, datatype = datatype, site=site_code[i], trange = trange, downloadonly=downloadonly, verbose = verbose
   endif
   if (site_code[i] eq 'ktb') then begin
      iug_load_aws_ktb, datatype = datatype, site=site_code[i], trange = trange,$
                        downloadonly=downloadonly, verbose = verbose
   endif    
  ;load of aws data at the Shigaraki sites
   if (site_code[i] eq 'sgk') then begin
      iug_load_aws_sgk, datatype = datatype, site=site_code[i], trange = trange,$
                        downloadonly=downloadonly, verbose = verbose
   endif 
endfor  
end


