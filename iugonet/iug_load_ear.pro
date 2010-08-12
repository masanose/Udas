;+
;
;Name:
;iug_load_ear
;
;Purpose:
;  Queries the Kyoto_RISH servers for all EAR data and loads data into
;  tplot format.
;
;Syntax:
;  iug_load_ear [ ,DATATYPE = string ]
;                [ ,PARAMETERS = string]
;                [ ,TRANGE = [min,max] ]
;                [ ,FILENAMES = string scalar or array ]
;                [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'trop_wind', 'trop_pwr', 'trop_spectral' etc.
;
;  PARAMETERS (I/O):
;    Set to wind parameters.  If not set, 'ear_wind_zon' is
;      assumed.  Returns cleaned input, or shows default.  
;  TRANGE (In):
;    Pass a time range a la TIME_STRING.PRO.
;  FILENAMES (In):
;    *PRESENTLY DISABLED* Pass user-defined file names (full paths to local data files).  These will
;      be read a la the RISH format, and the RISH server will not be queried.
;  VERBOSE (In): [1,...,5], Get more detailed (higher number) command line output.
;
;Code:
;A. Shinbori, 13/05/2010.
;
;Modifications:
;
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_ear, datatype = datatype, parameters = parameters, trange = trange, verbose = verbose

;******************
;keyword check:
;******************
;verbose
if ~keyword_set(verbose) then verbose=2
 
;**************************
;Load 'troposphere_wind' data by default:
;**************************
if ~keyword_set(datatype) then datatype='trop_wind'

;**************************
;Load 'parameters' data by default:
;**************************
if ~keyword_set(parameters) then parameters='zonal_wind_ear'

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
  ;======Load data================
  ;===============================
  ;load of ear tropsphere data
   iug_load_ear_trop,datatype=datatype,parameters=parameters,trange=trange
   
  ;load of ear ionosphere data
  ; iug_load_ear_iono,datatype=datatype,parameters=parameters,trange=trange 
   
  ;load of meteor radar data 
   iug_load_meteor_kot, datatype=datatype,parameters=parameters,trange=trange
   
  ;load of MF radar data 
   iug_load_MF_pam, datatype=datatype,parameters=parameters,trange=trange
   
end


