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
;
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_mu, datatype = datatype,parameter = site_or_param,$
                 downloadonly=downloadonly, trange=trange, verbose=verbose

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

                 
  ;===============================
  ;======Load data of MU=========
  ;===============================
  for i=0, n_elements(datatypes)-1 do begin
  ;load of MU tropsphere data
   if datatypes[i] eq 'troposphere' then begin
      iug_load_mu_trop_nc, datatype = datatypes[i], downloadonly=downloadonly, trange=trange, verbose=verbose
   endif 
   
   ;load of MU meteor wind data
   ;if datatype eq 'meteor_wind' then begin
    ;  iug_load_mu_meteor_txt, datatype = datatype, parameter = parameter2, trange = trange
  ; endif
   endfor  
   
end


