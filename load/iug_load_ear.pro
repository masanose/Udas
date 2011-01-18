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
;Data Availability:
;  Please check the following homepage of the time schedule of field-aligned irregularity (FAI) observation 
;  before you analyze the FAI data using this software. 
;  http://www.rish.kyoto-u.ac.jp/ear/data-fai/index.html#data
;
;Code:
;A. Shinbori, 13/05/2010.
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
  
pro iug_load_ear, datatype = datatype, parameter1 = site_or_param, parameter2 = parameters, trange = trange, verbose = verbose

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
  ;======Load data of EAR=========
  ;===============================
  ;load of ear tropsphere data
   if datatype eq 'troposphere' then begin
      iug_load_ear_trop_nc, datatype = datatype, trange = trange
   endif 
   if datatype eq 'e_region' then begin
      iug_load_ear_iono_er_nc, datatype = datatype, parameter = site_or_param, trange = trange
   endif 
   if datatype eq 'ef_region' then begin
      iug_load_ear_iono_efr_nc, datatype = datatype, parameter = site_or_param, trange = trange
   endif  
   if datatype eq 'v_region' then begin
      iug_load_ear_iono_vr_nc, datatype = datatype, parameter = site_or_param, trange = trange
   endif
   if datatype eq 'f_region' then begin
      iug_load_ear_iono_fr_nc, datatype = datatype, parameter = site_or_param, trange = trange
   endif 
   
;******************************
;print of acknowledgement:
;******************************
print, '****************************************************************
print, 'Acknowledgement'
print, '****************************************************************
print, 'If you acquire EAR data, we ask that you acknowledge us'
print, 'in your use of the data. This may be done by including text' 
print, 'such as EAR data provided by Research Institute for Sustainable' 
print, 'Humanosphere of Kyoto University. We would also appreciate receiving' 
print, 'a copy of the relevant publications.'
  
end


