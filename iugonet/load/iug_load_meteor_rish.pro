;+
;
;NAME:
;iug_load_meteor_rish
;
;PURPOSE:
;  Queries the RISH servers for the meteor observation data (netCDF format) taken by 
;  the meteor wind radar (MWR) at Kototabang and Serpong and loads data into tplot format.
;
;SYNTAX:
; iug_load_meteor_rish, datatype = datatype, site=site, parameter = parameter, $
;                       downloadonly=downloadonly, trange=trange, verbose=verbose
;
;KEYWOARDS:
; datatype = Observation data type. For example, iug_load_meteor_rish, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;   site  = Observatory code name.  For example, iug_load_meteor_rish, site = 'ktb'.
;           The default is 'all', i.e., load all available stations.
;  length = Data length '1-day' or '1-month'. For example, iug_load_meteor_rish, length = '1_day'.
;           A kind of parameters is 2 types of '1_day', and '1_month'.
; parameter = Data parameter. For example, iug_load_meteor_rish, parameter = 'h2t60min00'. 
;             A kind of parameters is 5 types of 'h2t60min00', 'h2t60min30', 'h4t60min00', 'h4t60min00', 'h4t240min00'.
;             The default is 'h2t60min00'. 
;  trange = (Optional) Time range of interest  (2 element array), if
;          this is not set, the default is to prompt the user. Note
;          that if the input time range is not a full day, a full
;          day's data is loaded.
;  /downloadonly, if set, then only download the data, do not load it
;                 into variables.
;
;
;CODE:
; A. Shinbori, 19/09/2010.
;
;MODIFICATIONS:
; A. Shinbori, 24/03/2011.
; A. Shinbori, 11/05/2011.
; A. Shinbori, 28/05/2012.
; A. Shinbori, 24/07/2012.
; A. Shinbori, 19/01/2013.
;  
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-


pro iug_load_meteor_rish, datatype = datatype, $
   site=site, $
   parameter = parameter, $
   length = length, $
   downloadonly=downloadonly, $
   trange=trange, $
   verbose=verbose

;**********************
;Verbose keyword check:
;**********************
if (not keyword_set(verbose)) then verbose=2

 
;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='thermosphere'


;*****************************
;Load '1_day' data by default:
;*****************************
if (not keyword_set(length)) then length='1_day'


;****************
;Parameter check:
;****************

;--- all parameters (default)
parameter_all = strsplit('h2t60min00 h2t60min30 h4t60min00 h4t60min30 h4t240min00',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)

print, parameters


;****************
;Site code check:
;****************
;--- all sites (default)
site_code_all = strsplit('bik ktb srp',' ', /extract)

;--- check site codes
if (not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

for i=0, n_elements(site_code)-1 do begin
  if site_code[i] eq 'bik' then iug_load_meteor_bik_nc, datatype = datatype, parameter = parameter, length=length, $
                                                        downloadonly=downloadonly, trange=trange, verbose=verbose
  if site_code[i] eq 'ktb' then iug_load_meteor_ktb_nc, datatype = datatype, parameter = parameter, length=length, $
                                                        downloadonly=downloadonly, trange=trange, verbose=verbose
  if site_code[i] eq 'srp' then iug_load_meteor_srp_nc, datatype = datatype, parameter = parameter, length=length, $
                                                        downloadonly=downloadonly, trange=trange, verbose=verbose
endfor
end
