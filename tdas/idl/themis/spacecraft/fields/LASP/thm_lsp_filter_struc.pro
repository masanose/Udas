;+
;HEAD:
;    function thm_lsp_filter_struc, data, dt, f_low, f_high
;                        
;PURPOSE:
;    This routine filters the data with the band [f_low, f_high]. The data to be
;    filtered must be continuously sammpled with the given sample rate. If the
;    routine exit unsuccessfully, -1 will be
;    retured.
;
;ARGUMENTS
;    data:        NEEDED. The data to be filtered. It must be a structure from a
;                 tplot variable.
;    dt:          NEEDED. The sample period of DATA.
;    f_low:       NEEDED. The lower bound of the filter, in units of Hz.
;    f_high:      NEEDED. The upper bound of the filter, in units of Hz.
;
;HISTORY:
;    2009-05-03: writen by Jianbao Tao, in LASP/CU.
;
;-

function thm_lsp_filter_struc, data, dt, f_low, f_high

;check if the argument list is complete
con1 = n_elements(data) eq 0
con2 = n_elements(f_low) eq 0
con3 = n_elements(f_high) eq 0
con4 = n_elements(dt) eq 0
con = con1 + con2 + con3 + con4
if con ge 1 then begin
  print, 'THM_LSP_FIELD_FILTER: ' + $
          'At least one argument is missing. Please check the'+$
          'help of the routine for required arguments.'
  return, -1
endif

;check the type of data
if size(data,/type) ne 8 then begin
  print, 'THM_LSP_FIELD_FILTER: ' + $
         'The data to be filtered must be a structure from a tplot variable.'
  return, -1
endif

ncomps = (size(data.y,/dim))[1]
for i = 0, ncomps-1 do begin
   print, 'filtering component: ', i
   field = data.y[*,i]
   field = thm_lsp_filter_fft(field,dt, f_low, f_high)
   data.y[*,i] = field
endfor

return, data

end

