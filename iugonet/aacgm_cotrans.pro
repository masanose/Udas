;+
;Procedure: aacgm_cotrans,
;  aacgm_cotrans
;  
;Purpose:
;  This procedure converts from the geopraphical to AACGM coordinates.
;  The AACGM exists in the following dyrectory.
;  /tdas_5_20/idl/ssl_general_cotrans/aacgm/
;  This is a sample code for IUGONET analysis software.
; Written by: Atsuki Shinbori
; -
;HISTORY:
;$LastChangedBy: A.Shinbori $
;$LastChangedDate: 2010-06-28 $

pro aacgm_cotrans

   aacgmidl

   station_name="ABK AAE ABG ASP AAA API KAK MMB EAR MU"
   station_name_all = strsplit(station_name, ' ', /extract)
   
   in_glat=[68.36,9.03,18.64,-23.76,43.25,-13.81,36.23,43.91,-0.20,34.85]
   in_glong=[18.82,38.77,72.87,133.88,76.92,188.23,140.19,144.19,100.32,136.10]
   
   out_mlat=fltarr(10)
   out_mlong=fltarr(10)
   
   for i=0,9 do begin
   ;the call of cnv_aacgm here converts from geographic to geomagnetic
   cnv_aacgm,in_glat(i),in_glong(i),0,u,v,10
   out_mlat(i)=u
   out_mlong(i)=v
   
   if out_mlong(i) lt 0 then out_mlong(i)=360+out_mlong(i)
   ;print of all the input and output data
   print,station_name_all(i),in_glat(i),in_glong(i),"   GG_LAT and GG_LONG"
   print,station_name_all(i),out_mlat(i),out_mlong(i),"   GM_LAT and GM_LONG"
   
   endfor
   
  !p.multi=[0,1,2]
  
   ;plot of all the input and output data
   plot,in_glong,in_glat,psym=7,xtitle= "Geogaphical Longitude [deg]",$
        ytitle= "Geogaphical Latitude [deg]",xrange=[0,360],yrange=[-90,90],yticks=4
   plot,out_mlong,out_mlat,psym=5,xtitle= "AACGM Longitude [deg]",$
        ytitle= "AACGM Latitude [deg]",xrange=[0,360],yrange=[-90,90],yticks=4

end