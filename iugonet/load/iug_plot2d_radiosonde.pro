;+
;
;NAME:
;  iug_plot2d_radiosonde
;
;PURPOSE:
;  Generate several height profiles from the radiosonde data. 
;
;SYNTAX:
;  iug_plot2d_radiosonde, datatype = datatype, site=site
;
;KEYWOARDS:
;  datatype = Observation data type. For example, iug_plot2d_radiosonde, datatype = 'troposphere'.
;            The default is 'troposphere'. 
;  site = Radiosonde observation sites.  
;         For example, iug_plot2d_radiosonde,site = 'sgk'.
;         The default is 'sgk'.
;
;CODE:
;  A. Shinbori, 30/05/2013.
;  
;MODIFICATIONS:
;
;  
;ACKNOWLEDGEMENT:
; $LastChangedBy:  $
; $LastChangedDate:  $
; $LastChangedRevision:  $
; $URL $
;-

pro iug_plot2d_radiosonde,datatype=datatype, site=site

;***************
;Datatype check:
;***************
if not keyword_set(datatype) then datatype='troposphere'

;****************
;Site code check:
;****************
;--- all sites (default)
site_code_all = strsplit('drw gpn ktb ktr sgk srp',' ', /extract)

;--- check site codes
if (not keyword_set(site)) then site='all'
site_code = thm_check_valid_name(site, site_code_all, /ignore_case, /include_all)

print, site_code

;Set up the window size of radiosonde data plot:
window ,0, xsize=1400,ysize=700,TITLE='IUGONET radiosonde data:'

;================================================
;Get the radiosonde data from tplot variable and 
;plot the hight profile of each parameter:
;================================================
param_name=tnames('iug_radiosonde_*')
tmp=''
site_number=0
for i=0, n_elements(param_name)-1 do begin
   split_para=strsplit(param_name[i],'_',/extract)
   if split_para[2] ne tmp then site_number=site_number+1
   tmp = split_para[2]
endfor
!P.Multi = [0, 5,site_number ,0, 0]
for i=0,n_elements(param_name)-1 do begin
   get_data,param_name[i],data=d,LIMITS=LIMITS
   split_para=strsplit(param_name[i],'_',/extract)
   ;if split_para[2] eq 'sgk' then window ,0, xsize=1400,ysize=700,TITLE='Shigaraki',/RETAIN
   
   for j=0, n_elements(d.x)-1 do begin 
      if split_para[3] eq 'press' then begin
         if j eq 0 then plot,d.y[j,*],d.v, xtitle='Pressure [hPa]' ,ytitle='Height [km]',$
                             charsize=1.5,title= split_para[2],yrange=[0,40]
         oplot,d.y[j,*],d.v
      endif 
      if split_para[3] eq 'temp' then begin
         if j eq 0 then plot,d.y[j,*],d.v, xtitle='Temperature [degree C]' ,ytitle='Height [km]',$
                             charsize=1.5,title= split_para[2],yrange=[0,40] 
         oplot,d.y[j,*],d.v
      endif 
      if split_para[3] eq 'rh' then begin
         if j eq 0 then plot,d.y[j,*],d.v, xtitle='Relative humidity [%]' ,ytitle='Height [km]',$
                             charsize=1.5,title= split_para[2],xrange=[0,100],yrange=[0,40] 
         oplot,d.y[j,*],d.v
      endif 
      if split_para[3] eq 'uwnd' then begin
         if j eq 0 then plot,d.y[j,*],d.v, xtitle='Zonal wind [m/s]' ,ytitle='Height [km]',$
                             charsize=1.5,title= split_para[2],xrange=[-50,50],yrange=[0,40]
         oplot,d.y[j,*],d.v
      endif 
       if split_para[3] eq 'vwnd' then begin
         if j eq 0 then plot,d.y[j,*],d.v, xtitle='Meridional wind [m/s]' ,ytitle='Height [km]',$
                             charsize=1.5,title= split_para[2],xrange=[-50,50],yrange=[0,40]
         oplot,d.y[j,*],d.v
      endif 
   endfor
endfor

end