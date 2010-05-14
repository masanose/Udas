;+
;Procedure: fac_crib
;
;Purpose:  A crib on showing how to transform into field aligned coordinates

;Notes:
;
; $LastChangedBy: jimm $
; $LastChangedDate: 2008-05-13 18:09:41 -0700 (Tue, 13 May 2008) $
; $LastChangedRevision: 3081 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/cotrans/special/fac/fac_crib.pro $
;-



; Example of FAC-Xgse matrix generation and rotation
timespan, '2007-03-23'

thm_load_fgm,probe = 'c', coord = 'gse'

;smooth the Bfield data appropriately
tsmooth2, 'thc_fgs_gse', 601, newname = 'thc_fgs_gse_sm601'

;make transformation matrix
fac_matrix_make, 'thc_fgs_gse_sm601', other_dim='xgse', newname = 'thc_fgs_gse_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'thc_fgs_gse_sm601_fac_mat', 'thc_fgs_gse', newname = 'thc_fgs_facx'

tplot, ['thc_fgs_gse', 'thc_fgs_gse_sm601', 'thc_fgs_facx']
tlimit,'2007-03-23/10:00:00','2007-03-23/13:00:00'

print, 'Just ran an example of FAC-Xgse matrix generation and rotation'

stop

; Example of FAC-Rgeo matrix generation and rotation
timespan, '2007-03-23'
thm_load_state,probe='c'
thm_load_fgm,probe = 'c', coord = 'gse'

;smooth the Bfield data appropriately
tsmooth2, 'thc_fgs_gse', 601, newname = 'thc_fgs_gse_sm601'

;make transformation matrix
fac_matrix_make, 'thc_fgs_gse_sm601',other_dim='rgeo', pos_var_name='thc_state_pos', newname = 'thc_fgs_gse_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'thc_fgs_gse_sm601_fac_mat', 'thc_fgs_gse', newname = 'thc_fgs_rgeo'

tplot, ['thc_fgs_gse', 'thc_fgs_gse_sm601', 'thc_fgs_rgeo']
tlimit,'2007-03-23/10:00:00','2007-03-23/13:00:00'

print, 'Just ran an example of FAC-Rgeo matrix generation and rotation'

stop

; Example of FAC-Phigeo matrix generation and rotation
timespan, '2007-03-23'
thm_load_state,probe='c'
thm_load_fgm,probe = 'c', coord = 'gse'

;smooth the Bfield data appropriately
tsmooth2, 'thc_fgs_gse', 601, newname = 'thc_fgs_gse_sm601'

;make transformation matrix
fac_matrix_make, 'thc_fgs_gse_sm601',other_dim='phigeo', pos_var_name='thc_state_pos', newname = 'thc_fgs_gse_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'thc_fgs_gse_sm601_fac_mat', 'thc_fgs_gse', newname = 'thc_fgs_phigeo'

tplot, ['thc_fgs_gse', 'thc_fgs_gse_sm601', 'thc_fgs_phigeo']
tlimit,'2007-03-23/10:00:00','2007-03-23/13:00:00'

print, 'Just ran an example of FAC-Phigeo matrix generation and rotation'

stop

; Example of FAC-Phism matrix generation and rotation
timespan, '2007-03-23'
thm_load_state,probe='c'
thm_load_fgm,probe = 'c', coord = 'gse'

;smooth the Bfield data appropriately
tsmooth2, 'thc_fgs_gse', 601, newname = 'thc_fgs_gse_sm601'

;make transformation matrix
fac_matrix_make, 'thc_fgs_gse_sm601',other_dim='Phism', pos_var_name='thc_state_pos', newname = 'thc_fgs_gse_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'thc_fgs_gse_sm601_fac_mat', 'thc_fgs_gse', newname = 'thc_fgs_phism'

tplot, ['thc_fgs_gse', 'thc_fgs_gse_sm601', 'thc_fgs_phism']
tlimit,'2007-03-23/10:00:00','2007-03-23/13:00:00'

print, 'Just ran an example of FAC-Phism matrix generation and rotation'

stop

; Example of FAC-Ygsm matrix generation and rotation
timespan, '2007-03-23'
thm_load_state,probe='c'
thm_load_fgm,probe = 'c', coord = 'gse'

;smooth the Bfield data appropriately
tsmooth2, 'thc_fgs_gse', 601, newname = 'thc_fgs_gse_sm601'

;make transformation matrix
fac_matrix_make, 'thc_fgs_gse_sm601',other_dim='ygsm', pos_var_name='thc_state_pos', newname = 'thc_fgs_gse_sm601_fac_mat'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'thc_fgs_gse_sm601_fac_mat', 'thc_fgs_gse', newname = 'thc_fgs_ygsm'

tplot, ['thc_fgs_gse', 'thc_fgs_gse_sm601', 'thc_fgs_ygsm']
tlimit,'2007-03-23/10:00:00','2007-03-23/13:00:00'

print, 'Just ran an example of FAC-Ygsm matrix generation and rotation'

stop

;If the timestamps of your data are not monotonic you may have problems doing rotation
;This is an example of how to correct non-monotonic timestamps.  This code sorts
;and removes duplicates

timespan,'2008-03-01'
thm_load_state,probe='e'
thm_load_fgm,probe='e',coord='gse'

get_data,'the_fgs_gse',data=d

idx = sort(d.x)

d.x = d.x[idx]
d.y = d.y[idx,*]

idx = where(d.x[1L:n_elements(d.x)-1L]-d.x[0L:n_elements(d.x)-2L] gt 0.) 

d2 = {x:d.x[idx],y:d.y[idx,*]}

store_data,'the_fgs_gse',data=d2

tsmooth2, 'the_fgs_gse', 601, newname = 'the_fgs_sm'

fac_matrix_make, 'the_fgs_sm', other_dim='xgse', newname = 'the_fgs_fac'

;transform Bfield vector (or any other) vector into field aligned coordinates
tvector_rotate, 'the_fgs_fac', 'the_fgs_sm', newname = 'the_fgs_rot'

tplot, ['the_fgs_gse','the_fgs_rot']

stop

end