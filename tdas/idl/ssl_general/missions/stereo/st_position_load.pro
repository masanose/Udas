pro st_position_load,  source_options=source_options,trange=trange,probe=probes,attitude=attitude

coord='GSE'

if not keyword_set(source_options) then begin
    stereo_init

    source_options = file_retrieve(/struct)
    source_options.local_data_dir = !stereo.local_data_dir  ; root_data_dir()+'stereo/'
    source_options.remote_data_dir = 'http://www.srl.caltech.edu/STEREO2/'  ;  'behind/position_behind_2006_GSE.txt
    source_options.ignore_filedate =1 ; All files on remote system are updated daily (even if no differences).
    source_options.min_age_limit = 24*3600d  ; check no more often than once every 24 hours
endif
mystereo = source_options
;mystereo.remote_data_dir = 'http://www.srl.caltech.edu/STEREO2/Position/'  ;  'behind/position_behind_2006_GSE.txt


if not keyword_set(probes) then probes = ['a','b']

res = 3600l*24 *365.25    ; one year resolution in the files
tr = timerange(trange)
n = ceil((tr[1]-tr[0])/res)  > 1
dates = dindgen(n)*res + tr[0]
;tr = time_string(timerange(trange),prec=-5)
;tr = time_struct(timerange(trange))
;years = tr.year
;ny = (year[1]-year[0]) + 1

for p=0,n_elements(probes)-1 do begin
   probe = probes[p]
;   pref = 'st'+probe+'_' + (keyword_set(burst) ? '_b' : '')
   case probe of
     'a' :  path = 'Position/ahead/position_ahead_YYYY_'+coord+'.txt'
     'b' :  path = 'Position/behind/position_behind_YYYY_'+coord+'.txt'
   endcase
   nv = 3
   tlabel = '_pos_'+coord
   scale = 6400.
   if keyword_set(attitude) then begin
     coord = 'RTN'
     case probe of
       'a' :  path = 'Pointing/ahead/pointing_ahead_YYYY_'+coord+'.txt'
       'b' :  path = 'Pointing/behind/pointing_behind_YYYY_'+coord+'.txt'
     endcase

     nv = 9
     tlabel = '_RotMat_SC>'+coord
     scale = 1.
   endif

   relpathnames= time_string(dates,tformat= path)
   relpathnames = relpathnames[uniq(relpathnames)]

   files = file_retrieve(relpathnames,_extra = mystereo)


   fformat = {year:0l, doy:0l,  sod:0l, flag:0,  pos:dblarr(nv)}
   data = 0
   for i=0,n_elements(files)-1 do $
      data = read_asc(files[i],format=fformat,append=data)

   tstr = replicate( time_struct(0d), n_elements(data) )
   tstr.year = data.year  & tstr.date=data.doy & tstr.fsec=data.sod & tstr.month=1
   tdbl = time_double(tstr)

   store_data,'st'+probe+tlabel,data={x:tdbl,y:transpose(data.pos/scale)},dlimit={ystyle:2}
   if keyword_set(attitude) && attitude eq 2 then begin
      nd = n_elements(data)
      par=0
      dprint,euler_rot_matrix(dummy,param=par)
      str_element,/add,par,'chi2',0.
      pars = replicate(par,nd)
      if probe eq 'b' then rt = [[-1d,0,0],[0,0,-1],[0,-1,0]]
      if probe eq 'a' then rt = [[-1d,0,0],[0,0,1],[0,1,0]]
      minres=1e-8
      for j = 0,nd-1 do begin
         dprint,dwait=10,j,' of ',nd
         r = rt ## reform(data[j].pos,3,3)
         fit,dummy,r,param=par,minres=minres,chi2=chi2 ,/silent ;,/testname  ;,verbose=dlevel
         par.chi2 = chi2
;         set_zeros,par
         par.the mod= 360  & par.phi mod= 360  & par.psi mod= 360
         pars[j] = par
      endfor
      store_data,'st'+probe+'_euler_angles_SC2>'+coord,data={x:tdbl,y:[[pars.phi],[pars.the],[pars.psi]]}
;      stop
   endif
   if keyword_set(attitude) && attitude eq 3 then begin
      nd = n_elements(data)
      par = 0
      dprint,euler_rot_matrix(dummy,param=par)
      par.eulpar[*]=.5   ; initial condition
      str_element,/add,par,'chi2',0.
      pars = replicate(par,nd)
      rt = identity(3,/double)
;      rt = [[-1d,0,0],[0,-1,0],[0,0,1]]
      minres=1e-8
      for j = 0,nd-1 do begin
         r = rt ## reform(data[j].pos,3,3)
         fit,dummy,r,param=par,minres=minres,/silent,chi2=chi2  ;,/testname  ;,verbose=dlevel
         par.chi2 =chi2
         dprint,dwait=10,j,' of ',nd,chi2
;         set_zeros,par
;         par.the mod= 360  & par.phi mod= 360  & par.psi mod= 360
         par.eulpar /= sqrt(total(par.eulpar^2))
         pars[j] = par
      endfor
      store_data,'st'+probe+'_euler_param_SC>'+coord,data={x:tdbl,y:transpose(pars.eulpar)},dlimit={yrange:[-1,1],ystyle:2}
      store_data,'st'+probe+'_euler_param_SC>'+coord+'_chi2',data={x:tdbl,y:pars.chi2},dlimit={ystyle:2,ylog:1}
;      stop
   endif
endfor


end

