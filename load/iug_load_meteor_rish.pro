;+
;
;Name:
;iug_load_meteor_rish
;
;Purpose:
;  Queries the Kyoto_RISH renkei2 servers for the kototabang and serpong data 
;  and loads data into tplot format.
;
;Syntax:
; iug_load_meteor_rish, datatype = datatype, site=site, parameters = parameters, $
;                       downloadonly=downloadonly, trange=trange, verbose=verbose
;
;Keywords:
; datatype = Observation data type. For example, iug_load_meteor_rish, datatype = 'thermosphere'.
;            The default is 'thermosphere'. 
;   site  = Observatory code name.  For example, iug_load_meteor_rish, site = 'ktb'.
;           The default is 'all', i.e., load all available stations.
; parameters = Data parameter. For example, iug_load_meteor_rish, parameter = 'h2t60min00'. 
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
;Code:
;  A. Shinbori, 26/01/2011.
;
;Modifications:
;
;  
;  
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-


pro iug_load_meteor_rish, datatype = datatype, site=site, parameters = parameters, $
                           downloadonly=downloadonly, trange=trange, verbose=verbose



;**************
;keyword check:
;**************
if (not keyword_set(verbose)) then verbose=2
 
;************************************
;Load 'thermosphere' data by default:
;************************************
if (not keyword_set(datatype)) then datatype='thermosphere'

;****************
;Parameter check:
;****************
if (not keyword_set(parameters)) then parameters='h2t60min00'

print, parameters
;***********
;site codes:
;***********
;--- all sites (default)
site_code_all = strsplit('ktb srp',' ', /extract)

;--- check site codes
if(not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

for i=0, n_elements(site_code)-1 do begin
  if (site_code[i] eq 'ktb') && (parameters ne 'h4t240min00') then iug_load_meteor_ktb_nc, datatype = datatype, parameters = parameters, $
                                                        downloadonly=downloadonly, trange=trange, verbose=verbose
  if site_code[i] eq 'srp' then iug_load_meteor_srp_nc, datatype = datatype, parameters = parameters, $
                                                        downloadonly=downloadonly, trange=trange, verbose=verbose
endfor

end
