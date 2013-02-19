;+
;	PROCEDURE overlay_map_irio
;
; :DESCRIPTION:
;    Plot a 2-D irio images on the plot window set up by map_set.
;
; :PARAMS:
;    datvn:   tplot variable names (as strings) to be plotted
;
; :KEYWORDS:
;    time:    Set the time (UNIX time) to plot a 2-D scan for
;    position:  Set the location of the plot frame in the plot window
;    erase:   Set to forcibly erase the plot window before plotting data
;    clip:    Set to scale in to get a magnified map
;    geo_plot:  Set to plot in the geographical coordinates
;    notimelabel: Set to prevent the time label from appearing on the plot
;    nocolorscale: Set to surpress drawing the color scale 
;    colorscalepos: Set the position of the color scale in the noraml 
;                   coordinates. Default: [0.85, 0.1, 0.87, 0.45] 
;    pixel_scale: Set a values of range 0.0-1.0 to scale pixels drawn on a 2D map plot
;
; :AUTHOR:
; 	Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/01/11: Created
; 	2011/06/15: renamed to overlay_map_irio
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $;
;-

;----------------------------------------------------------
PRO overlay_map_obs, obs, position=position, $
    erase=erase, clip=clip, geomag=geomag, timefor_geomag=timefor_geomag, $
    charsize=charsize, psym=psym, symsize=symsize
    
;Check argument and keyword
npar=N_PARAMS()
IF npar LT 1 THEN RETURN

if size(obs, /type) ne 8 then return

;Size of characters
if ~keyword_set(charsize) then charsize=1.0
if ~keyword_set(psym) then psym=5
if ~keyword_set(symsize) then symsize=1.0

nstn=n_elements(obs.name)
  
;Loop for processing a multi-tplot vars
FOR istn=0L, nstn-1 DO BEGIN
    
    ;Set the plot position
    pre_position = !p.position
    IF KEYWORD_SET(position) THEN BEGIN
        !p.position = position
    ENDIF ELSE position = !p.position
            
    ;Draw the data
    stname = obs.name[istn]
    lat = obs.glat[istn]
    lon = obs.glon[istn]
      
    ;Convert to AACGM
    IF KEYWORD_SET(geomag) THEN BEGIN
        ts = time_struct(timefor_geomag)
        year = ts.year & yrsec = LONG((ts.doy-1)*86400. + ts.sod)
        glat = lat & glon = (lon+360.) MOD 360.
        hgt = 100.  ;***** 100 km *****;
        aacgmconvcoord, glat, glon, hgt, mlat, mlon, err, /TO_AACGM
        mlt = aacgmmlt( year, yrsec, mlon )
        lon = ( (mlt + 24.) MOD 24. ) * 180./12.
        lat = mlat
    ENDIF

    oplot, [lon, lon], [lat, lat], psym=psym, symsize=symsize
    xyouts, lon, lat, strupcase(stname), charsize=charsize
      
ENDFOR ;End of the loop for multi-tplot var

;Normal end
RETURN
  
END

