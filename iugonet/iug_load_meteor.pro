;+
;
;Name:
;iug_load_meteor
;
;Purpose:
;  Queries the Kyoto_RISH servers for all EAR data and loads data into
;  tplot format.
;
;Syntax:
; iug_load_meteor   [ ,DATATYPE = string ]
;                   [ ,PARAMETERS = string]
;                   [ ,TRANGE = [min,max] ]
;                   [ ,FILENAMES = string scalar or array ]
;                   [ ,<and data keywords below> ]
;
;Keywords:
;  DATATYPE = The type of data to be loaded. In this load program,
;             DATATYPEs are 'thermo_kot'etc.
;
;  PARAMETERS (I/O):
;    Set to wind parameters.  If not set, 'zonal_wind' is
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
;A. Shinbori, 17/06/2010.
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_meteor, datatype = datatype, site_or_param = site_or_param, trange = trange, verbose = verbose

;******************
;keyword check:
;******************
;verbose
if ~keyword_set(verbose) then verbose=2
 
;**************************
;Load 'troposphere_wind' data by default:
;**************************
if ~keyword_set(datatype) then datatype='kototabang'

;**************************
;Load 'parameters' data by default:
;**************************
if ~keyword_set(parameters) then parameters='zonal_wind_kot'

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
     
  ;load of MF radar data 
   if datatype eq 'kototabang' then begin 
      iug_load_meteor_kot, datatype=datatype,parameters=parameters,trange=trange
   endif else if datatype eq 'serpong' then begin
      iug_load_meteor_srp, datatype=datatype,parameters=parameters,trange=trange
   endif
   
end


