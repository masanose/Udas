;+
; Please note this routine is still in development
;CDF2TPLOT ,files,varnames=varnames,all=all
;
; record=record if only one record and not full cdf-file is requested
;
; $LastChangedBy: davin-win $
; $LastChangedDate: 2007-07-16 08:14:38 -0700 (Mon, 16 Jul 2007) $
; $LastChangedRevision: 1110 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/CDF/cdf2tplot.pro $
;-

pro cdf2tplot,files=files,prefix=prefix,midfix=midfix,midpos=midpos,suffix=suffix $
   ,varformat=varformat ,varnames=varnames2 $
   ,all=all,verbose=verbose, get_support_data=get_support_data, convert_int1_to_int2=convert_int1_to_int2 $
   ,record=record, tplotnames=tplotnames


dprint,dlevel=4,verbose=verbose,'$Id: cdf2tplot.pro 1110 2007-07-16 15:14:38Z davin-win $'
vb = keyword_set(verbose) ? verbose : 0

; Load data from file(s)
dprint,dlevel=5,verbose=verbose,'Starting CDF file load'

if not keyword_set(varformat) then var_type = 'data'
if keyword_set(get_support_data) then var_type = ['data','support_data']
cdfi = cdf_load_vars(files,varformat=varformat,var_type=var_type,/spdf_depend, $
     varnames=varnames2,verbose=verbose,record=record, convert_int1_to_int2=convert_int1_to_int2)

dprint,dlevel=5,verbose=verbose,'Starting load into tplot'
;  Insert into tplot format
cdf_info_to_tplot,cdfi,varnames2,all=all,prefix=prefix,midfix=midfix,midpos=midpos,suffix=suffix, $  ;bpif keyword_set(all) eq 0
       verbose=verbose,  tplotnames=tplotnames


dprint,dlevel=5,verbose=verbose,'Starting Clean up' ;bpif keyword_set(all) eq 0
tplot_ptrs = ptr_extract(tnames(/dataquant))
unused_ptrs = ptr_extract(cdfi,except=tplot_ptrs)
ptr_free,unused_ptrs

end



