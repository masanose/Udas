;+
;
;NAME:
;iug_load_mu
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for all the observation data taken by 
;  the Middle and Upper atmosphere (MU) radar at Shigaraki and loads data into
;  tplot format.
;
;SYNTAX:
;  iug_load_mu [ ,DATATYPE = string ]
;                [ ,TRANGE = [min,max] ]
;                [ ,FILENAMES = string scalar or array ]
;                [ ,<and data keywords below> ]
;
;KEYWOARDS:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'troposphere' etc.
; 
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;CODE:
; A. Shinbori, 09/19/2010.
;
;MODIFICATIONS:
; A. Shinbori, 03/24/2011.
; A. Shinbori, 08/08/2012.
; A. Shinbori, 04/10/2012.
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_mu, datatype = datatype, level = level, parameter = parameter, downloadonly=downloadonly, $
                 trange=trange, verbose=verbose

;******************
;keyword check:
;******************
;verbose
if (not keyword_set(verbose)) then verbose=2
 
;****************
;Datatype check:
;****************

;--- all datatypes (default)
datatype_all = strsplit('troposphere mesosphere ionosphere meteor rass fai',' ', /extract)

;--- check datatypes
if(not keyword_set(datatype)) then datatype='all'
datatypes = thm_check_valid_name(datatype, datatype_all, /ignore_case, /include_all)

print, datatypes

;****************
;Level check:
;****************

;--- all parameter (default)
level_all = strsplit('org scr',' ', /extract)

;--- check parameters_1
if (not keyword_set(level)) then level='all'
levels = thm_check_valid_name(level, level_all, /ignore_case, /include_all)

;****************
;Parameter check:
;****************

;--- all parameters (default)
parameter_all = strsplit('h1t60min00 h1t60min30 h2t60min00 h2t60min30',' ', /extract)

;--- check parameters
if(not keyword_set(parameter)) then parameter='all'
parameters = thm_check_valid_name(parameter, parameter_all, /ignore_case, /include_all)
                 
  ;===============================
  ;======Load data of MU=========
  ;===============================
  for i=0, n_elements(datatypes)-1 do begin
  ;load of MU tropsphere data
   if datatypes[i] eq 'troposphere' then begin
      iug_load_mu_trop_nc, datatype = datatypes[i], downloadonly=downloadonly, trange=trange, verbose=verbose
   endif 
   
  ;load of MU mesosphere data
   if datatypes[i] eq 'mesosphere' then begin
      iug_load_mu_meso_nc, datatype = datatypes[i], level = levels, downloadonly=downloadonly, trange=trange, verbose=verbose
      iug_load_mu_meso_wind_nc, datatype = datatypes[i], level = levels, downloadonly=downloadonly, trange=trange, verbose=verbose
   endif 
 
  ;load of MU ionosphere data
   if datatypes[i] eq 'ionosphere' then begin
      iug_load_mu_iono_drift_nc, datatype = datatypes[i], downloadonly = downloadonly, trange = trange, verbose = verbose
      iug_load_mu_iono_pwr_nc, datatype = datatypes[i], downloadonly = downloadonly, trange = trange, verbose = verbose
      iug_load_mu_iono_teti_nc, datatype = datatypes[i], downloadonly = downloadonly, trange = trange, verbose = verbose
   endif
  ;load of MU meteor data  
   if datatypes[i] eq 'meteor' then begin
      iug_load_mu_meteor_nc, datatype = datatypes[i], parameter =parameters, trange = trange,downloadonly=downloadonly, verbose = verbose
   endif
   endfor  
   
end


