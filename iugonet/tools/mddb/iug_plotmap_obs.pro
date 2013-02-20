;+
; PROCEDURE iug_plotmap_obs
;
; PURPOSE:
;		Draw a fan plot of SD data on the world map
;
;	:Params:
;    var:   tplot variable to be plotted
;
;	:Keywords:
;    noerase:     Set to plot data without erasing the screen 
;    clip:        Set to scale in to get a magnified map
;    position:    Set the location of the plot frame in the plot window
;    center_glat: geographical latitude at which a plot region is centered
;    center_glon: geographical longitude at which a plot region is centered
;    mltlabel:    Set to draw the MLT labels every 2 hour
;    lonlab:      a latitude from which (toward the poles) the MLT labels are drawn
;    force_scale: Forcibly put a given value in "scale" of map_set
;    geo_plot:    Set to plot in the geographical coordinates
;    coast:      Set to superpose the world map on the plot
;    nocolorscale: Set to surpress drawing the color scale 
;    colorscalepos: Set the position of the color scale in the noraml 
;                   coordinates. Default: [0.85, 0.1, 0.87, 0.45] 
;
; :EXAMPLES:
;   plot_map_sdfit, 'sd_hok_vlos_bothscat'
;   plot_map_sdfit, 'sd_hok_vlos_bothscat', center_glat=70., center_glon=180. 
;   
; :Author:
; 	Tomo Hori (E-mail: horit at stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/03/11: Created
; 	2011/06/15: Renamed to plot_map_sdfit
;
;-

PRO iug_plotmap_obs $
    , glatlim=glatlim, glonlim=glonlim $
    , query=query, charsize=charsize $
    , psym=psym, symsize=symsize $
    , noerase=noerase $
    , clip=clip $
    , position=position $
    , mltlabel=mltlabel $
    , lonlab=lonlab $
    , geomag=geomag $
    , coast=coast $
    , obs=obs

;glatlim=[20, 50]
;glonlim=[120, 150]
;query='MAGDAS'

if ~keyword_set(glatlim) then glatlim=[-90, 90]
if ~keyword_set(glonlim) then glonlim=[-180, 180]
if ~keyword_set(geomag) then geo_plot=1

slat=glatlim[0] 
nlat=glatlim[1]
wlon=glonlim[0]
elon=glonlim[1]

glatc=mean(glatlim)
glonc=mean(glonlim)

lats = indgen(19)*10-90
lons = indgen(24)*15-180
latnames=' '
lonnames=' '

;----- Search observatory -----;
iug_get_obsinfo, nlat=nlat, slat=slat, elon=elon, wlon=wlon, $
                 query=query, obs=obs

;----- Draw map -----;
map_set, glatc, glonc, /azim, limit=[slat, wlon, nlat, elon], /cont, /isotropic
map_continents
map_grid,lats=lats, lons=lons, latnames=latnames, lonnames=lonnames

;----- Draw observatory -----;
overlay_map_obs, obs, position=position, $
    erase=(~KEYWORD_SET(noerase)), clip=clip, geomag=geomag, $ 
    charsize=charsize, psym=psym, symsize=symsize


end

