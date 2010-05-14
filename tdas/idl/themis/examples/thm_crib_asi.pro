;+
;thm_crib_asi.pro
;usage:
; .run thm_crib_asi
;
;
;Written by Harald Frey
; $LastChangedBy: kenb-win2000 $
; $LastChangedDate: 2007-02-11 21:26:08 -0500 (Sun, 11 Feb 2007) $
; $LastChangedRevision: 379 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/trunk/idl/themis/examples/thm_crib_asi.pro $
;
;-

; Before starting IDL run the setup
; source /usr/local/setup/setup_themis
; (same as the idl/themis/setup_themis file in the thmsw distribution.)

; Load keograms for 2006-01-01
thm_init
timespan,'2006-01-01',1,/day
thm_load_ask,/verbose
print,' '
print,'Data exist for the following stations: '
tplot_names,'*ask*'
stop

; set up some options for tplot and plot full day
window
loadct,0
YLIM,'*ask*',0,255
ZLIM,'thg_ask_atha',0,2.e4
ZLIM,'thg_ask_inuv',0,8.e3
ZLIM,'thg_ask_pgeo',0,2.e4
ZLIM,'thg_ask_rank',0,1.e4
ZLIM,'thg_ask_whit',0,7.e3
tplot_options, 'title', 'THEMIS ASI Examples'
TPLOT,'*ask*'
stop

; zoom into just one hour of data
timespan,'2006-01-01/04:00:00',1,/hours
ZLIM,'thg_ask_atha',3.e3,1.e4
ZLIM,'thg_ask_inuv',3.e3,6.e3
ZLIM,'thg_ask_pgeo',1.e3,2.e4
ZLIM,'thg_ask_rank',1.e3,1.e4
ZLIM,'thg_ask_whit',2.e3,4.e3
TPLOT,['thg_ask_pgeo','thg_ask_rank','thg_ask_whit']
stop

	; load full resolution images for one station
timespan,'2006-01-01/04:00:00',1,/hours
thm_load_asi,site='rank',datatype='asf'
tplot,'thg_ask_rank thg_asf_rank'
window,1,xsize=256,ysize=256
options,'thg_asf_rank',irange=[3000,10000.]
ctime,/cut
stop

	; create line plot of intensity
get_data,'thg_asf_rank',data=d
totals=total(total(d.y,2),2)
store_data,'rank_tot',data={x:d.x,y:totals-min(totals)}
tplot,'rank_tot'
stop

	; create mosaic from full resolution data
thm_asi_create_mosaic,'2006-12-23/06:20:30',/verbose
stop

	; create mosaic from thumbnail data with many options
show_time='2008-02-10/05:50:00'
thm_asi_create_mosaic,show_time,/verbose,/thumb,$
    exclude=['kuuj','kian'],$	; show only these stations
    central_lon=-100,central_lat=60.,scale=2.9e7,$	; set area
    projection='AzimuthalEquidistant'			; map projection
stop

	; get position of one spacecraft
thm_load_state,probe='c',coord='gsm',suffix='_gsm'

	; trace to the ionosphere
ttrace2iono,'thc_state_pos_gsm',newname='thc_ifoot_geo',external_model='t89',par=2.0D,/km,$
    in_coord='gsm',out_coord='geo'
get_data,'thc_ifoot_geo',data=d

	; transform to Lat/Lon
lon = !radeg * atan(d.y[*,1],d.y[*,0])
lat = !radeg * atan(d.y[*,2],sqrt(d.y[*,0]^2+d.y[*,1]^2))
plots,lon,lat

	; label a specific time
min_diff=min(abs(d.x-time_double(show_time)),index)
	; show footprint
xyouts,lon[index]+0.5,lat[index]+0.1,'THEMIS-P2',/data,charsize=2
	; we fake a position for demonstration
plots,lon[index],lat[index],psym=2,symsize=2
plots,lon[index],lat[index],psym=4,symsize=2

	; set color table back
loadct2,34

end

