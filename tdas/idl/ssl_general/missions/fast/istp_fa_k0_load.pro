pro istp_fa_k0_load,types,trange=trange

istp_init
source = !istp
;local_dir = root_data_dir() + 'fast/' ; '/data/fast/
;remote_dir = 'http://cdaweb.gsfc.nasa.gov/data/fast/'

if not keyword_set(types) then types = ['ees','ies']

for i=0,n_elements(types)-1 do begin
     type = types[i]
     relpath = 'fast/'+type+'/'
     prefix = 'fa_k0_'+type+'_'
     ending = '_v01.cdf'
     relpathnames = file_dailynames(relpath,prefix,ending,/YEARDIR,trange=trange)

     files = file_retrieve(relpathnames,_extra=source)
     if keyword_set(downloadonly) then continue
     cdf2tplot,file=files,all=all,verbose=verbose ,prefix = 'istp_fa_'    ; load data into tplot variables
endfor


end
