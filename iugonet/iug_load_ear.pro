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
;A. Shinbori, 24/08/2010.
;
;Acknowledgment:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-
  
pro iug_load_ear, trange = trange, verbose = verbose

;******************
;keyword check:
;******************
;verbose
if ~keyword_set(verbose) then verbose=2
 
  ;===============================
  ;======Load data================
  ;===============================
  ;load data of EAR standard observation of the tropsphere 
  ;wind velocity of zonal, meridional and vertical components:
   iug_load_ear_trop,datatype='trop_wind',parameters='zonal_wind_ear',trange=trange
   iug_load_ear_trop,datatype='trop_wind',parameters='meridional_wind_ear',trange=trange
   iug_load_ear_trop,datatype='trop_wind',parameters='vertical_wind_ear',trange=trange
   
  ;Echo power for each beam:
   iug_load_ear_trop,datatype='trop_pwr',parameters='pwr_beam1',trange=trange
   iug_load_ear_trop,datatype='trop_pwr',parameters='pwr_beam2',trange=trange
   iug_load_ear_trop,datatype='trop_pwr',parameters='pwr_beam3',trange=trange
   iug_load_ear_trop,datatype='trop_pwr',parameters='pwr_beam4',trange=trange
   iug_load_ear_trop,datatype='trop_pwr',parameters='pwr_beam5',trange=trange
   
  ;Spectral width for each beam:
   iug_load_ear_trop,datatype='trop_spec_width',parameters='sw_beam1',trange=trange
   iug_load_ear_trop,datatype='trop_spec_width',parameters='sw_beam2',trange=trange
   iug_load_ear_trop,datatype='trop_spec_width',parameters='sw_beam3',trange=trange
   iug_load_ear_trop,datatype='trop_spec_width',parameters='sw_beam4',trange=trange 
   iug_load_ear_trop,datatype='trop_spec_width',parameters='sw_beam5',trange=trange
      
  ;load data of EAR standard observation of the ionosphere
  
  ; iug_load_ear_iono,datatype=datatype,parameters=parameters,trange=trange 
   
   
end


