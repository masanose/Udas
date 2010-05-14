;+
;
; Procedure: THM_SST_PSEB
;
;
;
;VERSION:
;  $LastChangedBy: $
;  $LastChangedDate: $
;  $LastChangedRevision:  $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/spacecraft/particles/SST/thm_sst_psef.pro $
;-

function thm_sst_pseb,time,index=index,probe=probe,times=times,$
                      badbins2mask=badbins2mask,_extra=ex

data_cache,'th'+probe+'_sst_raw_data',data,/get

if not keyword_set(data) then begin
   dprint,dlevel=0,'No data loaded for probe: ',probe
   return,0
endif

if ~ptr_valid(data.seb_064_time) then begin
   dprint,dlevel=0,'No valid data of requested type for probe: ',probe
   return,0
end

if keyword_set(times) then return, *data.seb_064_time

dptr = data.seb_064_data
dim = size(/dimension,*dptr)
dist = thm_sst_dist3d_16x64(/elec,probe=probe)

dist.project_name = 'THEMIS '+strupcase(probe)
dist.data_name = 'SST Electron Full Burst distribution'
dist.magf = !values.f_nan

if n_elements(index) eq 0 then begin
    if n_elements(time) eq 0 then   ctime,time,npoints=1
    index=round( interp(dindgen(dim[0]),*data.seb_064_time,time) )
endif

dprint,dlevel=5,index

dist.index = index
dist.time = (*data.seb_064_time)[index]
dist.end_time = dist.time+3
dist.data= thm_part_decomp16((*data.seb_064_data)[index,*,*])
dist.cnfg= (*data.seb_064_cnfg)[index]
dist.atten = (*data.seb_064_atten)[index]
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


dist = thm_sst_remove_sunpulse(dist,badbins2mask=badbins2mask,_extra=ex)

return,dist
end
