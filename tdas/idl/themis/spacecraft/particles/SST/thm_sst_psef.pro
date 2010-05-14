;+
;
; Procedure: THM_SST_PSIF
;
;  For documentation on sun contamination correction keywords that
;  may be passed in through the _extra keyword please see:
;  thm_sst_remove_sunpulse.pro or thm_crib_sst_contamination.pro
;
;
;VERSION:
;  $LastChangedBy: davin-win $
;  $LastChangedDate: 2009-01-05 12:31:12 -0800 (Mon, 05 Jan 2009) $
;  $LastChangedRevision: 4304 $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/spacecraft/particles/SST/thm_sst_psef.pro $
;-

function thm_sst_psef,time,index=index,probe=probe,times=times,$
                      badbins2mask=badbins2mask,_extra=ex

data_cache,'th'+probe+'_sst_raw_data',data,/get

if not keyword_set(data) then begin
   dprint,dlevel=0,'No data loaded for ',probe
   return,0
endif

if keyword_set(times) then return, *data.sef_064_time

dptr = data.sef_064_data
dim = size(/dimension,*dptr)
dist = thm_sst_dist3d_16x64(/elec,probe=probe)

dist.project_name = 'THEMIS '+strupcase(probe)
dist.data_name = 'SST Electron Full distribution'
dist.magf = !values.f_nan

if n_elements(index) eq 0 then begin
    if n_elements(time) eq 0 then   ctime,time,npoints=1
    index=round( interp(dindgen(dim[0]),*data.sef_064_time,time) )
endif

dprint,dlevel=5,index

dist.index = index
dist.time = (*data.sef_064_time)[index]
dist.end_time = dist.time+3
dist.data= thm_part_decomp16((*data.sef_064_data)[index,*,*])
dist.cnfg= (*data.sef_064_cnfg)[index]
dist.atten = (*data.sef_064_atten)[index]
dist.units_name='Counts'
dist.valid=1
dist.mass = 0.511e6/(2.9979e5)^2
dist.bins[12:15,*] = 0

if keyword_set(badbins2mask) then begin
   if array_equal(badbins2mask,1) then begin
      bad_ang = [0,16,32,48] ; default masking created by Davin
      badbins2mask = intarr(64)+1
      badbins2mask[bad_ang] = 0
   endif
endif

dist = thm_sst_remove_sunpulse(dist, badbins2mask=badbins2mask, _extra=ex)

return,dist
end
