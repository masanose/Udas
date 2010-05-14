;+
;  Procedure:  FILE_DAILYNAMES
;  Author: Davin Larson
;-

function file_dailynames,dir,prefix,suffix,trange=trange, $
    hour_res=hour_res,  $
    unique=unique,  $
    file_format=file_format, $
    dir_format=dir_format, $
    times = times, $
    resolution =res, $
    yyyy_mm_dir=yyyy_mm_dir, $
    yeardir=yeardir,$
    addmaster=addmaster


if not keyword_set(dir) then dir=''
if not keyword_set(prefix) then prefix=''
if not keyword_set(suffix) then suffix=''

;sep = path_sep()
sep = '/'       ; '\' is not needed even with WINDOWS!

if keyword_set(yeardir) then      dir_format='YYYY'+sep
if keyword_set(YYYY_MM_DIR) then  dir_format='YYYY'+sep+'MM'+sep

if not keyword_set(res) then res = 24l*3600
if keyword_set(hour_res) then begin   ; one hour resolution
   res = 3600l
   if not keyword_set(file_format) then file_format = 'YYYYMMDDhh'
endif

if not keyword_set(file_format) then file_format = 'YYYYMMDD'



tr = timerange(trange)

mmtr = floor( tr / res + [0d,.999d] )
n = (mmtr[1]-mmtr[0])  > 1
times = (dindgen(n) + mmtr[0]) * res
if keyword_set(addmaster) then times = [!values.d_nan,times]

dates = time_string( times , tformat=file_format)

if keyword_set(dir_format) then  datedir = time_string(times,tformat=dir_format)  else datedir=''

files = dir + datedir + prefix + dates + suffix

if keyword_set(unique) then begin
   s = sort(files)
   u = uniq(files[s])
   files = files[s[u]]
   times= times[s[u]]
endif


return,files

end
