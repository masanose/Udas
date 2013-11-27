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
; A. Shinbori, 25/09/2013.
; A. Shinbori, 27/11/2013.
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
parameter_all_1 = strsplit('h1t60min00 h1t60min30 h2t60min00 h2t60min30 '+$
                          'ie2e4b ie2e4c ie2e4d ie2rea ie2mya ie2myb ie2rta ie2trb iecob3 '+$
                          'ied101 ied103 ied108 ied110 ied201 ied202 ied203 iedb4a iedb4b '+$
                          'iedb4c iedc4a iedc4b iedc4c iede4a iede4b iede4c iede4d iedp01 '+$
                          'iedp02 iedp03 iedp08 iedp10 iedp11 iedp12 iedp13 iedp1s iedpaa '+$
                          'iedpbb iedpcc iedpdd iedpee iedpff iedpgg iedphh iedpii iedpjj '+$
                          'iedpkk iedpl2 iedpll iedpmm iedptt iedpyy iedpzz ieewb5 ieimga '+$
                          'ieimgb ieimgm ieimgt ieis01 iefai1 iefdi2 ieggmt iemb5i iemcb3 '+$
                          'iemdb3 iemdb5 iemdc3 iemy3a iemy3b iemy3c iemyb5 iensb5 iepbr1 '+$
                          'iepbr2 iepbr3 iepbr4 iepbr5 iepbrt ieper1 ieper2 ieper3 ieper4 '+$
                          'ieper5 ieper6 ieper7 ieper8 ieps3a ieps3b ieps3c ieps4a ieps4b '+$
                          'ieps4c ieps4d ieps4e ieps5a ieps5b ieps5c ieps6a ieps6b iepsb3 '+$
                          'iepsb4 iepsb5 iepsi1 iepsi5 iepsit iesp01 iess01 iess02 iess03 '+$
                          'iess04 iess05 iess2l iess3l iess4l iess8c iessb5 iesst2 iesst3 '+$
                          'iet101 iet102 ietest ietst2 ieto02 ieto03 ieto16 ietob3 ietob4 '+$
                          'ietob5 iey4ch iey4ct ieyo4a ieyo4b ieyo4c ieyo4d ieyo4e ieyo4f '+$
                          'ieyo4g ieyo5a ieyo5b ieyo5c ieyo5d ieyo5e ieyo5f ieyo5g ieyo5m '+$
                          'ifco02 ifco03 ifco04 ifco16 if5bd1 if5bd2 if5bd3 if5bd4 if5bd5 '+$
                          'if5be1 if5be2 if5be3 if5be4 if5be5 ifchk1 ifdp00 ifdp01 ifdp02 '+$
                          'ifdp03 ifdp0a ifdp0b ifdp0c ifdp0d ifdp1u ifdp1s ifdp1t ifdpll '+$
                          'ifdq01 ifdq02 ifim16 ifmb16 ifmc16 ifmd16 ifmf16 ifmy01 ifmy02 '+$
                          'ifmy03 ifmy04 ifmy05 ifmy99 ifmyc1 ifmyc2 ifmyc3 ifmyc4 ifmyc5 '+$
                          'ifmyc6 ifmyc7 ifmyca ifmycb ifmyt1 ifmyt2 ifmyt3 ifmyt4 ifmyt5 '+$
                          'ifmyu1 ifmyu2 ifmyu3 ifmyu4 ifmyu5 ifmyv1 ifpsi1 ifpsit ifss02 '+$
                          'iftes1 iftes2 iftes3 iftes5 iftes6 iftes7 iftes8 ifts01 ifts02 '+$
                          'ifts03 ifts04 ifts05 ifts06 ifts07',' ', /extract)

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
      iug_load_mu_fai_nc, datatype = datatypes[i], parameter1 =parameters_1, $
                           trange = trange, downloadonly=downloadonly, verbose = verbose
   endif
   endfor  
   
end


