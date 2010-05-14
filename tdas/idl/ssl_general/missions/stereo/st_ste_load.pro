;+
;Procedure: st_ste_load
;
;Purpose:  Loads stereo ste data
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;
;Example:
;   st_load_ste
;Notes:
;  This routine is (should be) platform independent.
;
;
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision: $
; $URL:$
;-
pro st_ste_load,type,all=all,files=files,trange=trange, $
    verbose=verbose,burst=burst,probes=probes, $
    source_options=source_options, $
    version=ver

if not keyword_set(source_options) then begin
    stereo_init
    source_options = !stereo
endif
mystereo = source_options

if not keyword_set(probes) then probes = ['a','b']
if not keyword_set(type) then type = ''
if not keyword_set(ver) then ver='V01'

res = 3600l*24     ; one day resolution in the files
tr = timerange(trange)
n = ceil((tr[1]-tr[0])/res)  > 1
dates = dindgen(n)*res + tr[0]

for i=0,n_elements(probes)-1 do begin
   probe = probes[i]
   pref = 'st'+probe+'_' + (keyword_set(burst) ? '_b' : '')
   case probe of
   'a' :  path = 'impact/level1/ahead/ste/YYYY/MM/STA_L1_STE_YYYYMMDD_'+ver+'.cdf'
   'b' :  path = 'impact/level1/behind/ste/YYYY/MM/STB_L1_STE_YYYYMMDD_'+ver+'.cdf'
   endcase

   relpathnames= time_string(dates,tformat= path)

   files = file_retrieve(relpathnames,_extra = mystereo)
   vfm = 'STE_spectra STE_mode STE_energy'

   cdf2tplot,file=files,varformat=vfm,all=all,verbose=!stereo.verbose ,prefix=pref , /convert_int1   ; load data into tplot variables

;   vname=pref+'Distribution'
;
;   get_data,vname,data=d
; ;  printdat,d,vname
;
;   d2 = {x:d.x,  y:total(d.y,2), v:d.v1}
;   store_data,pref+'s',data=d2,dlimit={spec:1,zlog:1,ylog:1}
endfor


end
