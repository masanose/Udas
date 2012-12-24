;+
;
;NAME:
;iug_load_radiosonde_rish
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for all the observation data taken by 
;  the radiosonde at several observation points and loads data into
;  tplot format.
;
;SYNTAX:
;  iug_load_radiosonde_rish [ ,DATATYPE = string ]
;                           [ ,SITE = string ]
;                           [ ,TRANGE = [min,max] ]
;                           [ ,FILENAMES = string scalar or array ]
;                           [ ,<and data keywords below> ]
;
;KEYWOARDS:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'DAWEX' and 'misc'.
;  SITE = The observation site. In this load program,
;             defualt is 'sgk'.
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;CODE:
; A. Shinbori, 19/12/2012.
;
;MODIFICATIONS:
; 
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_radiosonde_rish, datatype = datatype, $
  site = site, $
  downloadonly=downloadonly, $
  trange=trange, $
  verbose=verbose

;**************
;keyword check:
;**************
;verbose
if (not keyword_set(verbose)) then verbose=2
 
;***************
;Datatype check:
;***************

;--- all datatypes (default)
datatype_all = strsplit('DAWEX misc',' ', /extract)

;--- check datatypes
if(not keyword_set(datatype)) then datatype='all'
datatypes = thm_check_valid_name(datatype, datatype_all, /ignore_case, /include_all)

print, datatypes

;***********
;site check:
;***********

;--- all parameter (default)
site_all = strsplit('drw gpn ktb ktr sgk srp',' ', /extract)

;--- check parameters_1
if (not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_all, /ignore_case, /include_all)

print, site_code
                 
  ;======================================
  ;======Load data of radiosonde=========
  ;======================================
  for i=0, n_elements(datatypes)-1 do begin
     ;load of DAWEX radiosonde data
      if strupcase(datatypes[i]) eq 'DAWEX' then begin
         iug_load_radiosonde_dawex_nc, datatype = datatypes[i], site=site_code, downloadonly=downloadonly, trange=trange, verbose=verbose
      endif 
     ;load of MU mesosphere data
      if datatypes[i] eq 'misc' then begin
        ;iug_load_radiosonde_sgk_txt, datatype = datatypes[i], site=site_code, downloadonly=downloadonly, trange=trange, verbose=verbose
      endif 
   endfor  
end


