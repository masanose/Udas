;+
; NAME:
;    THM_ASI_CREATE_MOSAIC
;
; PURPOSE:
;    create mosaic with all THEMIS ASI
;
; CATEGORY:
;    None
;
; CALLING SEQUENCE:
;    THM_ASI_CREATE_MOSAIC,time
;
; INPUTS:
;    Time	like '2006-01-01/05:00:00'
;
; OPTIONAL INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    cal_files	calibration files if they do not need to be read
;    gif_out    create a gif-file
;    verbose    print some diagnostics
;    pgm_file   do not read CDF, but pgm-files
;    zbuffer    do in z-buffer, not on the screen
;    thumb      use thumbnails instead of full resolution images
;    exclude    string of station names that should not be plotted
;    show       string of station names that should only be plotted
;    top        top color to be used for polyfill
;    special    special treatment for not mapped thumbnail
;    scale               scale for map set
;    central_lon         geographic longitude of center of plot
;    central_lat         geographic latitude of center of plot
;    xsize               xsize of window
;    ysize               ysize of window
;    color_continent     shade of continent fill
;    color_background    shade of background
;    position=position   position of plot on window (normal coordinates)
;    noerase=noerase     do not erase current window (no effect if {x,y}size set
;    cursor     finish with cursor info, loop if cursor>1
;    projection projection for map set, MAP_PROJ_INFO, PROJ_NAMES=names
;    window     set window number
;    rotation	rotate map
;    full_minute	full minute for pgm
;    minimum_elevation	minimum elevation to plot in degrees
;    no_grid=no_grid	do not plot geomagnetic grid
;    no_midnight=no_midnight	do not plot midnight meridian
;    add_plot           stop because we want to add something
;    mask		mask certain parts of image
;    xy_pos             xy position
;    location		mark geographic location
;    no_color		do not load color table, use existing
;	 xy_cursor	create array of cursor selected values to pass to upper program
;
; OUTPUTS:
;    None
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    None
;
; RESTRICTIONS:
;    None
;
; EXAMPLE:
;    THM_ASI_CREATE_MOSAIC,'2006-01-01/05:00:00'
;    THM_ASI_CREATE_MOSAIC,'2007-03-01/04:00:00',/thumb,exclude='ekat'
;
; MODIFICATION HISTORY:
;    Written by: Harald Frey, 02/06/2007
;                based on example from Donovan/Jackel
;
;                2007-03-15, hfrey, thumbnails, keyword exclude
;                2007-03-27, hfrey, special treatment for not mapped thumbsnails
;                2007-12-21, jmm, added explicit set_plot,'z' for zbuffer
;                2008-07-21, jmm, added gif_dir, for output directory option
;                2009-06-17, hfrey, a few additions to make my life easier
;				 2009-11-10, cgabrielse, added xy_cursor keyword for sending cursor values up level
;
; NOTES:
;
; VERSION:
;   $LastChangedBy: jimmpc $
;   $LastChangedDate: 2009-11-11 10:24:21 -0800 (Wed, 11 Nov 2009) $
;   $LastChangedRevision: 6928 $
;   $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/ground/thm_asi_create_mosaic.pro $
;
;-

PRO THM_ASI_CREATE_MOSAIC,time,$
    cal_files=cal_files,$              ; calibration files already read
    gif_out=gif_out,$                  ; output in gif file
    verbose=verbose,$                  ; print debug messages
    pgm_file=pgm_file,$                ; read raw pgm files
    thumb=thumb,$                      ; force thumbnails over full images
    exclude=exclude,$                  ; exclude certain stations
    top=top,$                          ; set top value for image
    special=special,$                  ; treat 32x32 thumbnails
    show=show,$                        ; limit stations shown
    scale=scale,$                      ; scale for map set
    central_lat=central_lat,$          ; geographic latitude of center of plot
    central_lon=central_lon,$          ; geographic longitude of center of plot
    color_continent=color_continent,$  ; shade of continent fill
    color_background=color_background,$; shade of background
    position=position,$                ; position of plot on window (normal coordinates)
    xsize=xsize,$                      ; xsize of window
    ysize=ysize,$                      ; ysize of window
    noerase=noerase,$                  ; do not erase current window (no effect if {x,y}size set)
    zbuffer=zbuffer,$		       ; do it in z-buffer
    cursor=cursor,$		       ; finish with cursor info
    projection=projection,$	       ; map projection
    maxval=maxval,$		       ; brightness scaling of images
    minval=minval,$                    ; brightness scaling of images
    window=window,$      	       ; set window number
    rotation=rotation,$                ; rotate map away from North up
    full_minute=full_minute,$          ; process a full minute of pgm files
    minimum_elevation=minimum_elevation,$ ; set minimum elevation
    merge=merge,$		       ; merge full resolution and thumbnail images
    gif_dir=gif_dir,$                  ; An output directory for the gif output, default is the local working dir.
    force_map=force_map,$              ; force display of empty map
    no_grid=no_grid,$                  ; do not plot grid
    no_midnight=no_midnight,$          ; do not plot midnight meridian
    add_plot=add_plot,$                ; stop so we can add something
    mask=mask,$    			; mask part of image
    xy_pos=xy_pos,$			; mark specific x,y-location
    location=location,$			; mark geographic location=[long,lat] or [[lo,la],[lo,la],[lo,la]]
    no_color=no_color, $			; do not load color table
    xy_cursor=xy_cursor		; create an array that records the cursor output and passes it to upper program


	; input check
if keyword_set(verbose) then verbose=systime(1)
if (strlen(time) ne 19) then begin
  print,'Wrong time input'
  print,'Correct value like 2006-01-01/05:00:00'
  return
  endif
if keyword_set(xy_pos) then begin
  dd=size(xy_pos)
  if (dd[2] ne 2) then begin
      print,'XY_pos input wrong!'
      print,'Needs to be given like [[x1,x2,x3,x4,x5,...],[y1,y2,y3,y4,y5,...]]'
      return
      endif
  endif

	; check brightness scaling
if keyword_set(maxval) then begin
  if not keyword_set(minval) then begin
     print,'minval has to be set with maxval'
     return
     endif
  if not keyword_set(show) then begin
     print,'Show has to be set with maxval'
     return
     endif
  if n_elements(show) ne n_elements(maxval) then begin
     print,'N_elements of show and maxval have to match'
     return
  endif
  endif

	; strip time
res=time_struct(time)
year=res.year
month=res.month
day=res.date
hour=res.hour
minute=res.min
second=res.sec

	; setup
thm_init
timespan,time,1,/hour
thm_asi_stations,site,loc
if keyword_set(zbuffer) then map_scale=2.6e7 else map_scale=4.e7
if keyword_set(scale) then map_scale=scale
if not keyword_set(central_lon) then central_lon=255.
if not keyword_set(central_lat) then central_lat=63.
if not keyword_set(xsize) then xsize=700
if not keyword_set(ysize) then ysize=410
if not keyword_set(top) then top=254

	; characters
if keyword_set(zbuffer) then chars=1.15 else chars=1.5

	; some setup
if keyword_set(minimum_elevation) then minimum_elevation_to_plot=minimum_elevation else minimum_elevation_to_plot=8. ;degrees
if (keyword_set(thumb) and not keyword_set(special)) then n1=1024l else n1=256l*256l

     	; clean up before start
names=tnames('thg_as*')
if (names[0] ne '') then store_data,delete=names

	; read available data
thm_mosaic_array,year,month,day,hour,minute,second,strlowcase(site),$
        image,corners,elevation,pixel_illuminated,n_sites,verbose=verbose,$
        cal_files=cal_files,pgm_file=pgm_file,thumb=thumb,special=special,$
        show=show,exclude=exclude,full_minute=full_minute,merge=merge,$
        mask=mask

	; exclude unwanted sites
if keyword_set(exclude) then begin
  for i=0,n_elements(exclude)-1 do begin
    not_site=where(strlowcase(site) eq strlowcase(exclude[i]),count)
    if (count eq 0) then print,'Not a valid site: ',exclude[i] else begin
        corners[*,*,*,not_site]=!values.f_nan
        image[*,not_site]=!values.f_nan
        endelse
    endfor
  endif

	; fill variables
bytval=fltarr(n_sites)+1.
bitval=fltarr(n_sites)
if keyword_set(maxval) then begin
  for i=0,n_elements(maxval)-1 do begin
    index=where(strlowcase(site) eq strlowcase(show[i]))
    bytval[index]=maxval[i]
    bitval[index]=minval[i]
    endfor
  for i=0,n_sites-1 do image[*,i]=bytscl(image[*,i],min=bitval[i],max=bytval[i])
  endif else begin
  if keyword_set(special) then for i=0,n_sites-1 do bytval[i]=(median(image[0:1023,i]) > 1) else begin
     if keyword_set(full_minute) then begin
        for i=0,n_sites-1 do bytval[i]=(median(image[*,i,*]) > 1)
        for i=0,n_sites-1 do image[*,i,*]=((image[*,i,*]/bytval[i])*64.) < 254
        endif else begin ; prevent divide by zero
        for i=0,n_sites-1 do bytval[i]=(median(image[*,i]) > 1) ; prevent divide by zero
        mm_dumm=where(merge ne -1,count_mm_dumm)
        if (count_mm_dumm ne 0) then begin
           for dki=0,count_mm_dumm-1 do bytval[mm_dumm[dki]]=(median(image[0:1023,mm_dumm[dki]]) > 1)
           endif
        for i=0,n_sites-1 do image[*,i]=((image[*,i]/bytval[i])*64.) < 254
        endelse
     endelse
  endelse

	; no images found
if (max(bytval) eq 1.) then begin
  print,'No images for ',time
  if not keyword_set(force_map) then begin
     if keyword_set(gif_out) then gif_out=''
     heap_gc
     return
     endif
  endif

	; exclude unwanted sites
if keyword_set(exclude) then begin
  for i=0,n_elements(exclude)-1 do begin
    not_site=where(strlowcase(site) eq strlowcase(exclude[i]),count)
    if (count eq 0) then print,'Not a valid site: ',exclude[i] else begin
        corners[*,*,*,not_site]=!values.f_nan
        bytval[not_site]=!values.f_nan
        endelse
    endfor
  endif

	; search for midnight file
	; does not change much over one minute
if not keyword_set(no_midnight) then begin
  f=file_search('midnight.sav',count=midnight_count)
  if (midnight_count eq 1) then begin
    midlons=fltarr(40)+!values.f_nan
    ut_hour=float(hour)+float(minute)/60.+float(second)/3600.
    restore,f
    for i=0,39 do begin
      lon=interpol(findgen(141)+start_longitude,reform(midnight[i,*]),ut_hour)
      midlons[i]=lon[0]
      endfor
    bad=where(midlons gt 360.,count)
    if (count gt 0) then midlons[bad]=!values.f_nan
    endif
  endif else midnight_count=0

;=========================================================================
	; generate images for full minute
if keyword_set(full_minute) then begin
time_start=time

	; run through images
for ikk=0,19 do begin

;zbuffer needs to be set before the loadct call in thm_map_set,
;otherwise this bombs the second time through because of reset to 'x'
;later in this program, jmm 21-dec-2007
if(keyword_set(zbuffer)) then set_plot, 'z'

; set up the map
thm_map_set,scale=map_scale,$
     central_lat=central_lat,$           ; geographic latitude of center of plot
     central_lon=central_lon,$           ; geographic longitude of center of plot
     color_continent=color_continent,$   ; shade of continent fill
     color_background=color_background,$ ; shade of background
     position=position,$                 ; position of plot on window (normal coordinates)
     xsize=xsize,$                       ; xsize of window
     ysize=ysize,$                       ; ysize of window
     noerase=noerase,$                   ; do not erase current window (no effect if {x,y}size set
     zbuffer=zbuffer,$
     projection=projection,$
     window=window,$
     rotation=rotation,$
     no_color=no_color

	; normal filling
for pixel=0l,n1-1l do begin
  for i_site=0,n_sites-1 do begin
    if ((pixel_illuminated[pixel,i_site] eq 1) and $
           (elevation[pixel,i_site] gt minimum_elevation_to_plot)) then $
       polyfill,corners[pixel,[0,1,2,3,0],0,i_site],$
            corners[pixel,[0,1,2,3,0],1,i_site],color=image[pixel,i_site, ikk] < top
    if keyword_set(special) then if ((spec_illu[pixel*64 < (n1-1),i_site] eq 1) and $
           (elevation[pixel*64 < (n1-1),i_site] gt minimum_elevation_to_plot)) then begin
       for ijk=0,63 do polyfill,corners[pixel*64+ijk < (n1-1),[0,1,2,3,0],0,i_site],$
            corners[pixel*64+ijk < (n1-1),[0,1,2,3,0],1,i_site],color=image[pixel*64+ijk < (n1-1),i_site] < top
;       stop
       endif
    endfor
  endfor

	; finish map
return_lons=1
return_lats=1
if not keyword_set(no_grid) then thm_map_add,invariant_lats=findgen(4)*10.+50.,invariant_color=210,$
    invariant_linestyle=1,/invariant_lons,return_lons=return_lons,$
    return_lats=return_lats
time=time_string(time_double(time_start)+ikk*3)

xyouts,0.005,0.018,time,color=0,/normal,charsize=chars
xyouts,0.005,0.060,'THEMIS-GBO ASI',color=0,/normal,charsize=chars

	; plot midnight file
if (not keyword_set(no_midnight) and midnight_count eq 1) then $
   plots,smooth(midlons-360.,5),findgen(40)+40.,color=255,/data

	; gif output
if keyword_set(gif_out) then begin
   If(keyword_set(gif_dir)) Then gdir = gif_dir Else gdir = './';jmm, 21-jul-2008
   tvlct,r,g,b,/get
   img=tvrd()
	; strip time because it changes through the loop
   res=time_struct(time)
   year=res.year
   month=res.month
   day=res.date
   hour=res.hour
   minute=res.min
   second=res.sec
   out_name='MOSA.'+year+'.'+month+'.'+day+'.'+hour+'.'+minute+'.'+second+'.gif'
   write_gif,gdir+out_name,img,r,g,b
   print,'Output in ',out_name
   gif_out=out_name
   endif

if keyword_set(zbuffer) then begin
     zbuffer=tvrd()
     device,/close
     set_plot,'x'
   endif

endfor
endif else begin	; end of full_minute loop
;=========================================================================

;zbuffer needs to be set before the loadct call in thm_map_set,
;otherwise this bombs the second time through because of reset to 'x'
;later in this program, jmm 21-dec-2007
if(keyword_set(zbuffer)) then set_plot, 'z'

	; set up the map
thm_map_set,scale=map_scale,$
     central_lat=central_lat,$           ; geographic latitude of center of plot
     central_lon=central_lon,$           ; geographic longitude of center of plot
     color_continent=color_continent,$   ; shade of continent fill
     color_background=color_background,$ ; shade of background
     position=position,$                 ; position of plot on window (normal coordinates)
     xsize=xsize,$                       ; xsize of window
     ysize=ysize,$                       ; ysize of window
     noerase=noerase,$                   ; do not erase current window (no effect if {x,y}size set
     zbuffer=zbuffer,$
     projection=projection,$
     window=window,$
     rotation=rotation,$
     no_color=no_color

	; fill it
if keyword_set(special) then begin
  spec_illu=pixel_illuminated-2
  for i=0,n_elements(special)-1 do begin
    spec_stat=where(strlowcase(site) eq strlowcase(special[i]))
    spec_illu[*,spec_stat]=pixel_illuminated[*,spec_stat]
    endfor
  endif

	; normal filling
for pixel=0l,n1-1l do begin
  for i_site=0,n_sites-1 do begin
    if ((pixel_illuminated[pixel,i_site] eq 1) and $
           (elevation[pixel,i_site] gt minimum_elevation_to_plot)) then $
       polyfill,corners[pixel,[0,1,2,3,0],0,i_site],$
            corners[pixel,[0,1,2,3,0],1,i_site],color=image[pixel,i_site] < top
    if keyword_set(special) then if ((spec_illu[pixel*64 < (n1-1),i_site] eq 1) and $
           (elevation[pixel*64 < (n1-1),i_site] gt minimum_elevation_to_plot)) then begin
       for ijk=0,63 do polyfill,corners[pixel*64+ijk < (n1-1),[0,1,2,3,0],0,i_site],$
            corners[pixel*64+ijk < (n1-1),[0,1,2,3,0],1,i_site],color=image[pixel*64+ijk < (n1-1),i_site] < top
       endif
    endfor
  if (keyword_set(special) and pixel ge 1024) then break
  endfor

	; finish map
return_lons=1
return_lats=1
thm_map_add,invariant_lats=findgen(4)*10.+50.,invariant_color=210,$
    invariant_linestyle=1,/invariant_lons,return_lons=return_lons,$
    return_lats=return_lats,no_grid=no_grid
xyouts,0.005,0.018,time,color=0,/normal,charsize=chars
xyouts,0.005,0.060,'THEMIS-GBO ASI',color=0,/normal,charsize=chars

if keyword_set(verbose) then print,'After map: ',systime(1)-verbose,$
   ' Seconds'

	; search for midnight file
if (not keyword_set(no_midnight) and midnight_count eq 1) then $
   plots,smooth(midlons-360.,5),findgen(40)+40.,color=255,/data

	; stop so we can add something
if keyword_set(add_plot) then stop

	; gif output
if keyword_set(gif_out) then begin
   If(keyword_set(gif_dir)) Then gdir = gif_dir Else gdir = './'
   tvlct,r,g,b,/get
   img=tvrd()
   	; now add the secret code of input parameters
   img[40:43,0]=[13,251,117,239]
   	; time of mosaic
   img[0:6,0]=[year/100,year-(year/100)*100,month,day,hour,minute,second]
   	; thumb flag
   if keyword_set(thumb) then img[7,0]=1 else img[7,0]=0
   	; central_lon and lat of mosaic
   if (central_lon lt 0.) then central_lon=central_lon+360.
   img[8:12,0]=[fix(central_lon)/100,fix(central_lon)-(fix(central_lon)/100)*100,$
               fix((central_lon-fix(central_lon))*100),$
               fix(central_lat),fix((central_lat-fix(central_lat))*100)]
   	; map_scale
   res=strsplit(string(map_scale*1.e10),'e',/extract)
   img[13:15,0]=[fix(res[0]),fix((float(res[0])-fix(res[0]))*100),fix(res[1])-10]
	; xsize and ysize
   img[16:19,0]=[xsize/100,xsize-(xsize/100)*100,ysize/100,ysize-(ysize/100)*100]
  	; rotation
   if keyword_set(rotation) then begin
       if (rotation lt 0.) then rotation=rotation+360.
       img[20:22,0]=[fix(rotation/100),fix(rotation-(fix(rotation)/100)*100),$
             fix((rotation-fix(rotation))*100)]
       endif else img[20:22]=[0,0,0]
   	; minimum elevation
   img[23:24,0]=[fix(minimum_elevation_to_plot),$
       fix((minimum_elevation_to_plot-fix(minimum_elevation_to_plot))*100)]
   	; zbuffer
   if keyword_set(zbuffer) then img[25,0]=1 else img[25,0]=0
   	; code stations
   img[49,0]=n_sites
   for i1=0,n_sites-1 do begin
      case 1 of
      finite(bytval[i1]) eq 0: img[50+i1,0]=0
      bytval[i1] eq 1.: img[50+i1,0]=1
      bytval[i1] gt 1.: img[50+i1,0]=2
      endcase
      endfor
	; construct the name
   out_name='MOSA.'+string(year,'(i4.4)')+'.'+string(month,'(i2.2)')+'.'+$
       string(day,'(i2.2)')+'.'+string(hour,'(i2.2)')+'.'+string(minute,'(i2.2)')+$
       '.'+string(second,'(i2.2)')+'.gif'
   write_gif,gdir+out_name,img,r,g,b
   print,'Output in ',out_name
   gif_out=out_name
   endif

if keyword_set(location) then begin
   plots,location[0,*],location[1,*],psym=2
   endif

if keyword_set(zbuffer) then begin
   zbuffer=tvrd()
   device,/close
   set_plot,'x'
   endif

if keyword_set(cursor) then begin
   ss=size(cursor)
   xy_cursor=fltarr(cursor,4)
   if (ss[1] ne 2) then cursor=1
   for loop=1,cursor do begin
     print,'Point cursor on map!'
     cursor,x,y,/data
     wait,0.25
     res=convert_coord(x,y,/data,/to_device)
     print,'Location: ',res,x,y
     xy_cursor[loop-1L,*]=[res[0],res[1],x,y]
     endfor
   endif

; input like [[x1,x2,x3,x4,x5,...],[y1,y2,y3,y4,y5,...]]
if keyword_set(xy_pos) then begin
   dd=size(xy_pos)
   if (dd[0] eq 1) then begin
     res=convert_coord(xy_pos[0],xy_pos[1],/to_data,/device)
     print,'Location: ',xy_pos,res[0:1],format='(a12,2i5,2f10.3)'
     xy_pos_out=[xy_pos[0],xy_pos[1],res[0],res[1]]
     endif else begin
     xy_pos_out=fltarr(dd[1],4)
     res=convert_coord(xy_pos[*,0],xy_pos[*,1],/to_data,/device)
     for i1=0L,dd[1]-1L do begin
       print,'Location: ',xy_pos[i1,*],res[0:1,i1],format='(a12,2i5,2f10.3)'
       xy_pos_out[i1,*]=[xy_pos[i1,0],xy_pos[i1,1],res[0,i1],res[1,i1]]
       endfor
     endelse
   xy_pos=xy_pos_out
   endif

endelse	; single time
;=========================================================================

if keyword_set(verbose) then print,'Calculation took ',systime(1)-verbose,$
   ' Seconds'



heap_gc
;stop
end
