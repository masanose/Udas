;+
; In addition to the NRSQ site included in the standard THEMIS GMAG
; data distribution, Hans Gleisner (email: hgl@dmi.dk) from the Danish
; Meteorological Institute has made ground magnetometer data from 11
; other sites in Greenland available for the time periods: 1 december
; 2007 through 30 april 2008 and 1 january 2009 through 28 february
; 2009. To access these data, simply use the program thm_load_gmag
; (The greenland_data keyword is no longer necessary

; for example

thm_load_gmag, trange = ['2009-01-01', '2009-01-02']

; loads all sites for 1-jan-2009, icluding the greenland stations. The
; greenland sites are handled in the same way as the other GMAG sites. 
; The valid site names for the greenland data are
;          ['AMK',  'ATU',  'DMH', 'DNB', 'GDH',  'KUV', 'NAQ',  'NRD',  'SCO', $
;           'SKT',  'STF',  'SVS',  'THL',  'UMQ', 'UPN']
; corresponding to:
;          ['Tasiilaq', 'Attu', 'Danmarkshavn', 'Daneborg', 'Qeqertarsuaq', $
;           'Kullorsuaq', 'Narsarsuaq', $
;           'Nord', 'Ittoqqortoormiit', 'Maniitsoq', 'Kangerlussuaq', $
;           'Savissivik', 'Qaanaaq', 'Uummannaq', 'Upernavik']
;                  Note that the station 'naq' is the THEMIS GMAG
;                  station 'NRSQ'
;To get data for individual sites, use the site keyword:

thm_load_gmag, trange = ['2009-01-01', '2009-01-02'], site = ['amk', 'gdh']

;or

thm_load_gmag, trange = ['2009-01-01', '2009-01-02'],  site = ['amk gdh']

;etc... The other keyword inputs for thm_load_gmag,
;(/subtract_average, /subtract_median, /valid_names, etc...) still
;work. See THM_CRIB_GMAG in this directory for examples.

;If this data is used in a publication, please acknowledge the source: 
; "Acknowledgement: Magnetometer data provided by: Hans Gleisner,
; Danish Meteorological Institute

End


