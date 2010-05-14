;+
;Procedure: OMNI_HRO_LOAD
;
;Purpose:  Loads OMNI data
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;Example:
;   OMNI_HRO_load
;Notes:
;  This routine is still in development.
; Author: Davin Larson
;
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL: $
;-
pro omni_hro_load,type,files=files,trange=trange,verbose=verbose,downloadonly=downloadonly, $
      varformat=varformat,datatype=datatype, $
      res5min=res5min,res1min=res1min, $
      addmaster=addmaster,data_source=data_source, $
      tplotnames=tn,source_options=source

;if not keyword_set(datatype) then datatype = 'h0'

istp_init
if not keyword_set(source) then source = !istp

rstrings = ['1min','5min']
rstr = rstrings[ keyword_set(res5min)  ]


;if datatype eq 'k0'  then    pathformat = 'wind/mfi/YYYY/wi_k0_mfi_YYYYMMDD_v0?.cdf'
;if datatype eq 'h0'  then    pathformat = 'wind/mfi_h0/YYYY/wi_h0_mfi_YYYYMMDD_v0?.cdf'

pathformat = 'omni/hro_'+rstr+'/YYYY/omni_hro_'+rstr+'_YYYYMM01_v01.cdf

;if not keyword_set(varformat) then begin
;   if datatype eq  'k0' then    varformat = 'BGSEc PGSE'
;   if datatype eq  'h0' then    varformat = 'B3GSE'
;endif

relpathnames = file_dailynames(file_format=pathformat,trange=trange,/unique)

files = file_retrieve(relpathnames, _extra=source)

if keyword_set(downloadonly) then return

prefix = 'OMNI_HRO_'+rstr+'_'
cdf2tplot,file=files,varformat=varformat,verbose=verbose,prefix=prefix ,tplotnames=tn    ; load data into tplot variables

; Set options for specific variables

dprint,dlevel=3,'tplotnames: ',tn

options,/def,tn+'',/lazy_ytitle          ; options for all quantities
options,/def,strfilter(tn,'*_T',delim=' ') , max_value=5e6    ; set colors for the vector quantities
;options,/def,strfilter(tn,'*B*GSE* *B*GSM*',delim=' '),constant=0., labels=['Bx','By','Bz'] , ysubtitle = '[nT]'

end
