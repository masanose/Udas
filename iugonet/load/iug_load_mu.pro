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
;              [ ,LEVEL = string ]
;              [ ,PARAMETER1 = string ]
;              [ ,PARAMETER2 = string ]
;              [ ,TRANGE = [min,max] ]
;              [ ,FILENAMES = string scalar or array ]
;              [ ,<and data keywords below> ]
;
;KEYWOARDS:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'troposphere' etc.
;  LEVEL = The level of mesospheric data to be loaded. In this load program,
;             LEVELs are 'org' and 'scr'.
;  PARAMETER1 = The parameter of meteor and fai data to be loaded. In this load program,
;             PARAMETERs are 'h1t60min00','h1t60min30','h2t60min00','h2t60min30' and 'iemdc3'.
;  PARAMETER2 = The parameter of rass and fai data to be loaded. In this load program,
;             PARAMETERs are 'uwnd','pwr1','wdt1' 'dpl1','pn1' and so on.
;  LENGTH = The file type of meteor data to be loaded.
;             LENGTHs are '1-day' and '1-month'. Default is 1-day.
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
; A. Shinbori, 12/11/2012.
; A. Shinbori, 02/08/2013.
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_mu, datatype = datatype, level = level, length=length, $
                 parameter1 = parameter1, parameter2 = parameter2, downloadonly=downloadonly, $
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

;*******************
;Parameter-1 check:
;*******************

;--- all parameters (default)
parameter_all_1 = strsplit('h1t60min00 h1t60min30 h2t60min00 h2t60min30 iemdc3',' ', /extract)

;--- check parameters
if(not keyword_set(parameter1)) then parameter1='all'
parameters_1 = thm_check_valid_name(parameter1, parameter_all_1, /ignore_case, /include_all)

;*******************
;Parameter-2 check:
;*******************

;--- all parameters (default)
parameter_all_2 = strsplit('uwnd vwnd wwnd temp pwr1 pwr2 pwr3 pwr4 pwr5 dpl1 dpl2 dpl3 dpl4 dpl5 '+$
                           'wdt1 wdt2 wdt3 wdt4 wdt5 pn1 pn2 pn3 pn4 pn5',' ', /extract)

;--- check parameters
if(not keyword_set(parameter2)) then parameter2='all'
parameters_2 = thm_check_valid_name(parameter2, parameter_all_2, /ignore_case, /include_all)
                 
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
      iug_load_mu_meteor_nc, datatype = datatypes[i], parameter =parameters_1, length=length, trange = trange, downloadonly=downloadonly, verbose = verbose
   endif
   
  ;load of MU RASS data  
   if datatypes[i] eq 'rass' then begin
      iug_load_mu_rass_txt, datatype = datatypes[i], parameter =parameters_2, $
                            trange = trange, downloadonly=downloadonly, verbose = verbose
   endif

  ;load of MU FAI data  
   if datatypes[i] eq 'fai' then begin
      iug_load_mu_fai_txt, datatype = datatypes[i], parameter1 =parameters_1, parameter2 =parameters_2, $
                           trange = trange, downloadonly=downloadonly, verbose = verbose
   endif
   endfor  
   
end


