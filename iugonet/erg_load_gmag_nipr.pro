;+
; PROCEDURE: erg_load_gmag_nipr
;   to load the 1-sec resolution geomagnetic data from the NIPR site.
;   This procedure is an alias of iug_load_gmag_nipr.pro.
;
; KEYWORDS:
;   site  = Observatory name, example, iug_load_gmag_nipr, site='syo',
;           the default is 'all', i.e., load all available stations.
;           This can be an array of strings, e.g., ['syo', 'hus']
;           or a single string delimited by spaces, e.g., 'syo hus'.
;           Sites:  syo
;   datatype = Time resolution. '1sec' for 1 sec. (Not available.)
;   /downloadonly, if set, then only download the data, do not load it into variables.
;   trange = (Optional) Time range of interest  (2 element array).
;
; EXAMPLE:
;   erg_load_gmag_nipr, site='syo', $
;                        trange=['2003-11-20/00:00:00','2003-11-21/00:00:00']
;
; NOTE: See the rules of the road.
;
; Written by Y. Tanaka, August 13, 2010
;-

pro erg_load_gmag_nipr, site = site, datatype=datatype, $
        downloadonly=downloadonly, trange=trange, verbose=verbose

iug_load_gmag_nipr, site = site, datatype=datatype, $
        downloadonly=downloadonly, trange=trange, verbose=verbose

end
