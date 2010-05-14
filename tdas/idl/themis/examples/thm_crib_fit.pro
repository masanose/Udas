;+
;	Batch File: THM_CRIB_FIT
;
;	Purpose:  Demonstrate the loading, calibration, and plotting
;		of THEMIS FIT (On-Board E- and B-Field SpinFit) data.
;
;	Calling Sequence:
;	.run thm_crib_fit, or using cut-and-paste.
;
;	Arguements:
;   None.
;
;	Notes:
;	None.
;
;Written by John Bonnell
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:44:00 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7468 $
; $URL $
;-

; FIT On-Board SpinFit data example.

; set a few TPLOT options.
tplot_title = 'THEMIS FIT On-Board Spin Fit Examples'
tplot_options, 'title', tplot_title
tplot_options, 'xmargin', [ 15, 10]
tplot_options, 'ymargin', [ 5, 5]

; set the timespan and load the FIT data.
;timespan, '2007-06-30', 1.0, /day
timespan, '2008-05-15', 1.0, /day

thm_load_fit, level=1,datatype=['efs', 'fgs'],/verbose

; set the color table.
loadct2, 39

; the FIT data, as loaded by THM_LOAD_FIT isn't very usable or viewable,
; so run THM_CAL_FIT to break out the E and B fits with useful plotting options.

; plot the calibrated FIT data.
tplot, ['thc_efs', 'thc_fgs']

print, 'at each stop point type .c to continue with the crib'

stop

;tlimit, ['2007-06-30/00:45:00', '2007-06-30/16:30:00']
tlimit, ['2008-05-15/10:00:00', '2008-05-15/14:00:00']

print, 'now we zoomed in'


stop

tplot, 'th?_fgs th?_efs'

print, 'all probes plotted.' 
; Note only probes Charley, Delta, Echo have '
;print, 'booms deployed for good E-Field data.'

stop

thm_load_state, /get_support_data

thm_cotrans, 'th?_??s', out_suffix='_gsm', out_coord='gsm'

tplot, [ 'thc_efs',     'thc_fgs', $
         'thc_efs_gsm', 'thc_fgs_gsm']

print, 'we transformed both efs and fgs to gsm, adding _gsm suffix to result'
stop

thm_load_fit, level=1,datatype=['efs', 'fgs'], coord='gsm', suffix='_gsm'

tplot, [ 'thc_efs',     'thc_fgs', $
         'thc_efs_gsm', 'thc_fgs_gsm']

print, 'you can get the same result (with better plot labels) if you load'
print, 'specify the coord keyword to thm_load_fit.'
stop

thm_load_fgm, datatype=['fgs'],/verbose,  suffix='_l2', level = 'l2'
; L2 EFI not yet available...
;thm_load_efi, datatype=['efs'],/verbose, suffix='_l2' 

tplot, ['thc_fgs',  'thc_fgs_dsl_l2']

print, 'now we loaded the same data from level 2.  Note L2 data has a '
print, 'suffix to designate the coordinate.  '
print, 'EFI is not yet available directly from L2 files.'

end
