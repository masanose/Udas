pro st_mag_make_lowresfile,probe=probe,resolution=res

stereo_init
mysource=!stereo

if not keyword_set(probe) then begin
  st_mag_make_lowresfile, probe='a'
  st_mag_make_lowresfile, probe='b'
  return
endif

ver = 'V03'
type = ''
   case probe of
   'a' :  path = 'impact/level1/ahead/mag/????/??/STA_L1_MAG'+type+'_????????_'+ver+'.cdf'
   'b' :  path = 'impact/level1/behind/mag/????/??/STB_L1_MAG'+type+'_????????_'+ver+'.cdf'
   endcase

varf = 'B_SC B_RTN'
resstr = '1min'  & res = 60d
;resstr = '2sec'  & res = 2d


files = file_search(mysource.local_data_dir + path , count=nfiles)
for i=0l,nfiles-1 do begin
   infile = files[i]

   outfile = infile
   str_replace,outfile,'level1','lowres/'+resstr,/reverse_search
   str_replace,outfile,'_MAG','_MAG_'+resstr,/reverse_search
   str_replace,outfile,'.cdf','.sav',/reverse_search
   fi = file_info(infile)
   fo = file_info(outfile)
   if fo.mtime lt fi.mtime then begin
      dprint,'Creating: ',outfile
      cdf2tplot,files=infile,varformat=varf,verbose=vb,tplotnames=tn
      get_data,'B_SC',time1,B_SC
      get_data,'B_RTN',time2,B_RTN
      tmid = median(time1)
      day = 24d*3600d
      tmid = floor(tmid/day) * day
      w = where(time1 ge tmid and time1 lt (tmid+day),nw)
      B_SC  = reduce_timeres(time1[w],B_SC[w,*],res,newtime=time)
      B_RTN = reduce_timeres(time2[w],B_RTN[w,*],res,newtime=time)
      file_mkdir2,file_dirname(outfile),mode = '777'o
      save,file=outfile,time,B_SC,B_RTN
      file_chmod,outfile,'666'o
   endif else dprint,'Skipping ',outfile
endfor


end


