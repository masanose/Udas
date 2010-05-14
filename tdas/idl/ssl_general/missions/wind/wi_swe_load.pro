;+
;Procedure: WI_SWE_LOAD
;
;Purpose:  Loads WIND SWE data
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;Example:
;   wi_swe_load
;Notes:
;  This routine is still in development.
;
;
; $LastChangedBy: davin-win $
; $LastChangedDate: 2007-08-01 15:18:52 -0700 (Wed, 01 Aug 2007) $
; $LastChangedRevision: 1313 $
; $URL $
;-
pro wi_swe_load,type,files=files,trange=trange,verbose=verbose,downloadonly=downloadonly, $
      get_support_data=get_support_data,varformat=varformat,datatype=datatype, $
      tplotnames=tplotnames,source=source

   if not keyword_set(datatype) then datatype = 'k0'

   istp_init
   if not keyword_set(source) then source = !istp
   if datatype eq 'k0' then    pathformat = 'wind/swe/YYYY/wi_k0_swe_YYYYMMDD_v0?.cdf'
   if datatype eq 'h0' then    pathformat = 'wind/swe_h0/YYYY/wi_h0_swe_YYYYMMDD_v02.cdf'
   if datatype eq 'h1' then    pathformat = 'wind/swe_h1/YYYY/wi_h1_swe_YYYYMMDD_v01.cdf'

;   source.remote_data_dir = 'ftp://cdaweb.gsfc.nasa.gov/pub/'
;   source.remote_data_dir ='http://themis.ssl.berkeley.edu/data/
if not keyword_set(varformat) then begin
   if datatype eq 'k0'  then varformat= 'V_GSE THERMAL_SPD Np Alpha_Percent'    ; Ions
   if datatype eq 'h0'  then varformat= '*'                                     ; Electrons
endif

relpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)

files = file_retrieve(relpathnames, _extra=source)

if keyword_set(downloadonly) then return

prefix='wi_swe_'
cdf2tplot,file=files,varformat=varformat,verbose=verbose,prefix=prefix, tplotnames=tplotnames     ; load data into tplot variables
dprint,tplotnames

options,/def,strfilter(tplotnames,'*GSE*',/string), colors='bgr'


end
