;+
;Procedure: WI_MFI_LOAD
;
;Purpose:  Loads WIND fluxgate magnetometer data
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;Example:
;   wi_mfi_load
;Notes:
;  This routine is still in development.
; Author: Davin Larson
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2007-11-07 13:34:54 -0800 (Wed, 07 Nov 2007) $
; $LastChangedRevision: 1982 $
; $URL $
;-
pro wi_mfi_load,type,files=files,trange=trange,verbose=verbose,downloadonly=downloadonly, $
      varformat=varformat,datatype=datatype, $
      addmaster=addmaster,data_source=data_source,tplotnames=tn,source_options=source

if not keyword_set(datatype) then datatype = 'h0'

istp_init
if not keyword_set(source) then source = !istp

if datatype eq 'k0'  then    pathformat = 'wind/mfi/YYYY/wi_k0_mfi_YYYYMMDD_v0?.cdf'
if datatype eq 'h0'  then    pathformat = 'wind/mfi_h0/YYYY/wi_h0_mfi_YYYYMMDD_v0?.cdf'

if not keyword_set(varformat) then begin
   if datatype eq  'k0' then    varformat = 'BGSEc PGSE'
   if datatype eq  'h0' then    varformat = 'B3GSE'
endif

relpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)

files = file_retrieve(relpathnames, _extra=source, /last_version)

if keyword_set(downloadonly) then return

prefix = 'wi_'+datatype+'_mfi_'
cdf2tplot,file=files,varformat=varformat,verbose=verbose,prefix=prefix ,tplotnames=tn    ; load data into tplot variables

; Set options for specific variables

dprint,dlevel=3,'tplotnames: ',tn

options,/def,tn+'',/lazy_ytitle          ; options for all quantities
options,/def,strfilter(tn,'*GSE* *GSM*',delim=' ') , colors='bgr' , labels=['x','y','z']    ; set colors for the vector quantities
options,/def,strfilter(tn,'*B*GSE* *B*GSM*',delim=' '),constant=0., labels=['Bx','By','Bz'] , ysubtitle = '[nT]'

end
