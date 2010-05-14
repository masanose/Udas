;+
;Procedure: GOES_EP_LOAD
;
;Purpose:  Loads GOES MAG data
;
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;Example:
;   goes_ep_load,probe='11'
;Notes:
;  This routine is still in development.
; Author: Davin Larson
;
; $LastChangedBy: davin-win $
; $LastChangedDate: $
; $LastChangedRevision:  $
; $URL $
;-
pro goes_ep_load,trange=trange,verbose=verbose,downloadonly=downloadonly, $
      varformat=varformat,datatype=datatype, $
      probe=probe, $
      addmaster=addmaster,tplotnames=tn,source_options=source

if not keyword_set(probe) then probe = '11'
;if not keyword_set(datatype) then $
   datatype = 'k0'

istp_init
if not keyword_set(source) then source = !istp

dprint,dlevel=2,verbose=source.verbose,'Loading GOES ',probe,' EP data'

if datatype eq 'k0'  then begin
   case probe of
   '0':   pathformat = 'goes/0_ep8/YYYY/g0_k0_ep8_YYYYMMDD_v03.cdf'
   '6':   pathformat = 'goes/6_eps/YYYY/g6_k0_eps_YYYYMMDD_v02.cdf'
   '7':   pathformat = 'goes/7_eps/YYYY/g7_k0_eps_YYYYMMDD_v02.cdf'
   '8':   pathformat = 'goes/8_ep8/YYYY/g8_k0_ep8_YYYYMMDD_v03.cdf'
   '9':   pathformat = 'goes/9_ep8/YYYY/g9_k0_ep8_YYYYMMDD_v03.cdf'
   '11':  pathformat = 'goes/11_ep8/YYYY/goes11_k0_ep8_YYYYMMDD_v03.cdf'
   '12':  pathformat = 'goes/12_eps/YYYY/goes12_k0_eps_YYYYMMDD_v03.cdf'
   else:  pathformat = ''
   endcase
endif

if not keyword_set(pathformat) then begin
    dprint,'Not a valid probe'
    return
endif

if not keyword_set(varformat) then begin
   varformat = '*'
;   if datatype eq  'k0' then    varformat = 'BGSEc'
;   if datatype eq  'h0' then    varformat = '*'
;   if datatype eq  'h1' then    varformat = '*'
endif


relpathnames = file_dailynames(file_format=pathformat,trange=trange,addmaster=addmaster)

files = file_retrieve(relpathnames, _extra=source, /last_version)

if keyword_set(downloadonly) then return

prefix = 'goes'+probe+'_ep_'
cdf2tplot,file=files,varformat=varformat,verbose=source.verbose,prefix=prefix ,tplotnames=tn    ; load data into tplot variables

; Set options for specific variables

dprint,dlevel=3,'tplotnames: ',tn

del_data,strfilter(tn,'*PB5')

;options,/def,strfilter(tn,'*fl*x'),/ylog
;options,/def,strfilter(tn,'*GSE* *GSM*',delim=' '),/lazy_ytitle , colors='bgr'     ; set colors for the vector quantities
;options,/def,strfilter(tn,'*B*GSE* *B*GSM*',delim=' '), labels=['Bx','By','Bz'] , ysubtitle = '[nT]'



end
