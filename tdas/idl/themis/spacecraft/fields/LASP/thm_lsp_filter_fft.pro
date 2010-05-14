;+
;HEAD:
;    function thm_lsp_filter_fft, datain, dt, flow, fhigh
;                        
;PURPOSE:
;    This routine filters the input data with the band [flow, fhigh]. The data to
;    be filtered must be continuously sammpled with the given sample rate. If
;    the routine exit unsuccessfully, -1 will be retured.
;
;ARGUMENTS
;    datain: NEEDED. The data to be filtered. It must be a 1-D array.
;    dt: NEEDED. The sample period of DATAIN.
;    flow: NEEDED. The lower bound of the filter, in units of Hz.
;    fhigh: NEEDED. The upper bound of the filter, in units of Hz.
;
;HISTORY:
;    2009-05-03: Writen by Jianbao Tao, in LASP/CU.
;    2009-05-17: Fixed the edge effect due to sectioning original data.
;                Jianbao Tao, in CU/LASP
;
;-

function thm_lsp_filter_fft, datain, dt, flow, fhigh


;check if the argument list is complete
con1 = n_elements(datain) eq 0
con2 = n_elements(flow) eq 0
con3 = n_elements(fhigh) eq 0
con4 = n_elements(dt) eq 0
con = con1 + con2 + con3 + con4
if con ge 1 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' + $
         'At least one argument is missing. Please check the'+$
            'help of the routine for required arguments.'
   return, -1
endif

f_low = flow
f_high = fhigh
data = datain

;check the dimension of data
tmp1 = size(data,/dim)
if n_elements(tmp1) gt 1 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' + $
         'The data to be filtered must be a 1-D array.'
   return, -1
endif
;check the dimension of f_low
tmp1 = size(f_low,/dim)
if tmp1[0] ne 0 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' + $
         'The lower bound of the filter must be a scalar.'
   return, -1
endif
f_low >= 0
;check the dimension of f_high
tmp1 = size(f_high,/dim)
if tmp1[0] ne 0 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' +  $
         'The upper bound of the filter must be a scalar.'
   return, -1
endif
;check the dimension of dt
tmp1 = size(dt,/dim)
if tmp1[0] ne 0 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' + $
         'The sample rate of the data must be a scalar.'
   return, -1
endif

nyquist = long(0.5/dt)
f_high <= nyquist

;check if f_low < f_high
if f_low ge f_high then begin
  print, 'THM_LSP_DIGITAL_FILTER: ' + $
         'The lower bound of the filter must be less than the upper one.'
  return, -1
endif

; set parameters for digital_filter
f = [f_low,f_high] / double(nyquist)
dB = 50.d

;clean up all NAN's, if there are, in the data by interpolation.
npt = n_elements(data)
dumx = findgen(npt)
ind = where(~finite(data,/nan))
if ind[0] eq -1 then begin
   print, 'THM_LSP_DIGITAL_FILTER: ' + $
      'All data points are NaNs. Do nothing.'
   return, data
endif
data = interpol(data[ind],dumx[ind],dumx)

; Chop data into several sections to speed up convolution.
; The last section is treated differently from those in its front.
; The last section includes the last normal section and the remainder
; from the modulo operation:
;                    NPT MOD (2*NTERMS+1)
nterms = 8192. * 2.0 - 1.
nsec = long(npt / (2. * nterms + 1))
nsec >= 1
if nsec ge 2 then begin
   ista = lindgen(nsec) * (2. * nterms + 1)
   iend = lonarr(nsec)
   iend[0:nsec-2] = ista[0:nsec-2] + 2. * nterms
   if (npt - ista[nsec-1]) mod 2 eq 0 then iend[nsec-1] = npt - 2 $
            else iend[nsec-1] = npt - 1
   lastnterms = long((iend[nsec-1] - ista[nsec-1])/2.) 
   filter = digital_filter(f[0],f[1],dB,nterms,/double)  ; digital filter
                                                         ; for normal sections
   lastfilter = digital_filter(f[0],f[1],dB,lastnterms,/double) ; digital filter
                                                             ; for the last section.
endif else begin
   ista = lindgen(1)
   if npt mod 2 eq 0 then begin
      iend = lonarr(1) + npt - 2
      lastnterms = (npt - 1) / 2
   endif else begin
      iend = lonarr(1) + npt - 1
      lastnterms = npt / 2
   endelse
   lastfilter = digital_filter(f[0],f[1],dB,lastnterms,/double)
endelse

for i=0L, nsec-1 do begin
   field = data[ista[i]:iend[i]]
   if i lt nsec-1 then field = fft_convolution(field,filter,/double) $
            else field = fft_convolution(field,lastfilter,/double)
   data[ista[i]:iend[i]] = field
endfor

; Fix the edge due to sectioning the data as follows.
; First, take a 2-second long part which is centerred about the edge effect.
; Second, take the corresponding original data and filter it.
; Third, weigh the results from above and merge them. 
if nsec ge 2 then begin
   edgenterms = 8192. - 1.
   edgefilter = digital_filter(f[0], f[1], dB, edgenterms, /double)
   tmpx = dindgen(2*edgenterms + 1) * dt
   tmpx = tmpx -  median(tmpx)
   weighcoef1 = 1. / (1. + 1d10 * tmpx^12) ; for original data
   weighcoef2 = 1. - weighcoef1           ; for edge-effect data
   for i=0L, nsec-2 do begin
      edgesec = data[iend[i]-edgenterms:iend[i]+edgenterms]   ; get a section
                                                              ; with edge effect
      origsec = datain[iend[i]-edgenterms:iend[i]+edgenterms] ; original data
      origsec  = fft_convolution(origsec, edgefilter, /double)
      data[iend[i]-edgenterms:iend[i]+edgenterms] = $
            origsec * weighcoef1 + edgesec * weighcoef2
   endfor
endif


return, data
end
