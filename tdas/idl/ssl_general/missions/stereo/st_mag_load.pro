
;+
;Procedure: st_mag_load
;
;Purpose:  Loads stereo mag data
;keywords:
;   TRANGE= (Optional) Time range of interest  (2 element array).
;   /VERBOSE : set to output some useful info
;
;Example:
;   st_mag_load,probe='a'
;Notes:
;  This routine is (should be) platform independent.
;
;
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision: $
; $URL:$
;-
pro st_mag_load,all=all,trange=trange,verbose=verbose,version = ver,probes=probes,type = type

if not keyword_set(source_options) then begin
    stereo_init
    source_options = !stereo
endif
mystereo = source_options

vb =mystereo.verbose

if not keyword_set(probes) then probes = ['a','b']
if not keyword_set(type) then type='8Hz'
_type = ''
leveldir = 'level1/'
ext = '.cdf'

if type ne '8Hz' then begin
  leveldir = 'lowres/'+type+'/'
  _type = '_'+type
  ext='.sav'
endif

if not keyword_set(ver) then ver='V03'

res = 3600l*24     ; one day resolution in the files
tr = timerange(trange)
n = ceil((tr[1]-tr[0])/res)  > 1
dates = dindgen(n)*res + tr[0]

varformat =  'B_SC B_RTN'
for p=0,n_elements(probes)-1  do begin
   probe = probes[p]
   pref = 'st'+probe+'_' + (keyword_set(burst) ? '_b' : '')
   case probe of
   'a' :  path = 'impact/'+leveldir+'ahead/mag/YYYY/MM/STA_L1_MAG'+_type+'_YYYYMMDD_'+ver+ext
   'b' :  path = 'impact/'+leveldir+'behind/mag/YYYY/MM/STB_L1_MAG'+_type+'_YYYYMMDD_'+ver+ext
   endcase

   relpathnames= time_string(dates,tformat= path)

   files = file_retrieve(relpathnames,_extra = !stereo)

   if ext eq '.cdf' then begin
      cdf2tplot,tplotnames=tn,file=files,varformat=varformat,all=all,verbose=vb,midfix='_8Hz',midpos=1 ,prefix=pref  ;,/get_support   ; load data into tplot variables
   endif

   if ext eq '.sav' then  begin
      B_rtn_all = 0
      B_sc_all=0
      time_all=0
      for i=0,n_elements(files)-1 do begin
         file = files[i]
         if file_test(/regular,file) then begin
           dprint,dlevel=2,'Loading: ',file
           restore,file=file,verbose=verbose
           append_array,B_rtn_all,B_rtn
           append_array,B_SC_all,B_SC
           append_array,time_all,time
         endif else dprint,dlevel=1,'File not found: ',file
      endfor
      store_data,'st'+probe+'_B'+_type+'_SC',data={x:time_all,y:B_sc_all}
      store_data,'st'+probe+'_B'+_type+'_RTN',data={x:time_all,y:B_rtn_all}
      tn = tnames('st?_mag_*_B_*')
   endif

   options,/def,strfilter(tn,'*_B_*'),colors='bgr'
   options,/def,strfilter(tn,'*_B_*_RTN'),labels=['R','T','N']
   options,/def,strfilter(tn,'*_B_*_SC'),labels=['X','Y','Z']

endfor


end
