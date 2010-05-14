;+
;  $Id: thm_sst_pser.pro 3414 2008-08-19 20:00:22Z bckerr $
;-
function thm_sst_pser,time,index=index,probe=probe,times=times,$
                      badbins2mask=badbins2mask

data_cache,'th'+probe+'_sst_raw_data',data,/get

if not keyword_set(data) then begin
   dprint,dlevel=0,'No data loaded for ',probe
   return,0
endif


if keyword_set(times) then  return, *data.ser_mix_time

if n_elements(index) eq 0 then begin
    if n_elements(time) eq 0 then begin
        ctime,time
    endif
    index=round( interp(dindgen(n_elements(*data.ser_mix_time)),*data.ser_mix_time,time) )
endif

if n_elements(index) ne 1 then message,'time/index ranges not allowed yet'

mode = (*data.ser_mix_mode)[index]
ind  = (*data.ser_mix_index)[index]

case  mode of
  0:  begin          ; 6 angle mode
      dptr = { data: data.ser_006_data, $
               time: data.ser_006_time, $
               cnfg: data.ser_006_cnfg, $
               attn: data.ser_006_atten }
      dist = thm_sst_dist3d_16x6(/elec)
      end
  1:  begin          ; 1 angle mode
      dptr = { data: data.ser_001_data, $
               time: data.ser_001_time, $
               cnfg: data.ser_001_cnfg, $
               attn: data.ser_001_atten  }
      dist = thm_sst_dist3d_16x1(/elec)
      end
endcase


;dim = size(/dimension,*dptr)
;dist = dst.d3d
dist.project_name = 'THEMIS '+strupcase(probe)
dist.data_name  = 'SST Electron Reduced Distribution'
dist.magf = !values.f_nan


dprint,dlevel=3,'index=',index,'ind=',ind

dist.index = ind
dist.time = (*dptr.time)[ind]
dist.end_time = dist.time+3
dist.data= thm_part_decomp16((*dptr.data)[ind,*,*])
dist.cnfg= (*dptr.cnfg)[ind]
dist.atten = (*dptr.attn)[ind]
dist.units_name = 'Counts'
dist.valid=1
dist.mass = 0.511e6/(2.9979e5)^2

if keyword_set(badbins2mask) then begin
   bad_ang = badbins2mask
   if array_equal(badbins2mask, -1) then begin
      print,''
      dprint,'WARNING: BADBINS2MASK array is empty. No bins masked for ', $
                      'th'+probe,'_psef data.'
      print,''
   endif else begin
      dist.bins[*,bad_ang] = 0
   endelse
endif

return,dist
end
