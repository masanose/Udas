;+
;
;Name:
;iug_load_mu
;
;PURPOSE:
;  Queries the Kyoto_RISH servers for all the MU data and loads data into
;  tplot format.
;
;Syntax:
;  iug_load_mu [ ,DATATYPE = string ]
;                [ ,PARAMETERS = string]
;                [ ,TRANGE = [min,max] ]
;                [ ,FILENAMES = string scalar or array ]
;                [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'troposphere', 'meteor_wind' etc.
;
;  PARAMETERS (I/O):
;    Set to wind parameters.  If not set, 'uwnd' is
;      assumed.  Returns cleaned input, or shows default.  
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;A. Shinbori, 21/10/2010.
;
;Modifications:
;A. Shinbori, 25/11/2010.
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_mu, datatype = datatype, parameter = parameter2, trange = trange, verbose = verbose

;******************
;keyword check:
;******************
;verbose
if ~keyword_set(verbose) then verbose=2
 
;**************************
;Load 'troposphere_wind' data by default:
;**************************
if ~keyword_set(datatype) then datatype='troposphere'

;**************************
;Load 'parameters' data by default:
;**************************
if ~keyword_set(parameters) then parameters='uwnd'

;*****************
;Validate datatypes:
;*****************
vns = datatype
if size(datatype,/type) eq 7 then begin
  datatype=thm_check_valid_name(datatype,vns,/ignore_case,/include_all,/no_warning)
  if datatype[0] eq '' then return
endif else begin
  message,'DATATYPE must be of string type.',/info
  return
endelse
                 
  ;===============================
  ;======Load data of MU=========
  ;===============================
  ;load of MU tropsphere data
   if datatype eq 'troposphere' then begin
      iug_load_mu_trop_nc, datatype = datatype, trange = trange
   endif 
   
   ;load of MU meteor wind data
   if datatype eq 'meteor_wind' then begin
      iug_load_mu_meteor_txt, datatype = datatype, parameter = parameter2, trange = trange
   endif
      
   
end


