;+
;thm_crib_fgm.pro
;usage:
; .run thm_crib_fgm
;
;
;Written by Hannes Schwarzl and Ken Bromund
; $LastChangedBy: jimm $
; $LastChangedDate: 2010-04-07 14:44:00 -0700 (Wed, 07 Apr 2010) $
; $LastChangedRevision: 7468 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/examples/thm_crib_fgm.pro $
;
;-

;timespan, '6-8-17', 1
timespan, '2007-3-23', 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print, "-"
print, "- 'standardized' interface, with intermediate outputs saved"
print, "--> enter .c"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stop

thm_load_state,probe='a', /get_support_data

thm_load_fgm,lev=1,probe=['a'],/get_support_data,type='raw', suffix='_raw'

thm_cal_fgm,probe=['a'],datatype='fg?',in_suffix='_raw', out_suffix='_ssl'

tplot_options, 'title', 'THEMIS FGM Examples'
tplot, ['tha_fgl_raw', 'tha_fgl_ssl']

;  note: thm_contrans accepts probe and datatype keywords:
thm_cotrans,probe=['a'],datatype='fg?',in_suffix='_ssl', out_suffix='_dsl', out_coord='dsl'
;  note: thm_cotrans can also work directly with tplot names:
thm_cotrans,'tha_fg?',in_suf='_dsl',out_suf='_gse', out_c='gse'

thm_cotrans,'tha_fg?',in_suf='_gse', out_suf='_gsm', out_c='gsm'

tplot, ['tha_fgl_raw', 'tha_fgl_ssl', 'tha_fgl_dsl', 'tha_fgl_gse','tha_fgl_gsm']

stop

tplot, ['tha_fg?_gsm']

stop

store_data, 'Tha_fgl_dsl_gse', data=['tha_fgl_dsl', 'tha_fgl_gse']
 options, 'tha_fgl_gse', 'colors', [1, 3, 5]
tplot, 'Tha_fgl_dsl_gse'


; clean up support data
del_data, 'th?_fg?_hed th?_state'
tplot_names
; clean up support data, and intermediate outputs, and state data
;del_data, 'th?_fg?_* th?_state*'
;tplot_names


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print, "-"
print, "- next: same thing, but  without saving intermediate outputs"
print, "--> enter .c"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stop

del_data, 'th* Th*'

thm_load_state,probe='a', /get_support_data

thm_load_fgm,level='L1',probe=['a'],/get_support_data,type='raw'

thm_cal_fgm,probe=['a'],datatype='fg?'

thm_cotrans,probe=['a'],datatype='fg?', out_coord='dsl'

thm_cotrans,'tha_fg?', out_c='gse'

thm_cotrans,'tha_fg?', out_c='gsm'

tplot, ['tha_fg?']

; clean up support data
del_data, 'th?_fg?_hed'
tplot_names


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print, "-"
print, "- next: load, calibrate, and transform coordinates in one call"
print, "--> enter .c"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stop

del_data, 'th* Th*'

timespan, '2007-3-23', 1
; only load spin data needed for cotrans
thm_load_state, probe='a', datatype='spin*', /get_support_data
; default is 'l1 calibrated dsl': request 'l1 calibrated gsm'
thm_load_fgm,level=1, probe=['a'],datatype='fg?',coord='gsm'
tplot, 'tha_fg?'

;  another thm_cotrans example: when not doing globbing, you can
;  specify output tplot variable directly:
thm_cotrans, 'tha_fgl', 'tha_fgl_gse', out_coord='gse'

;; Since /get_support_data keyword was not given, but default action is
;; to calibrate: the necessary support data is retrieved and
;; cleaned up automatically by thm_load_fgm.
;; if you want it to stick around, then give the /get_support_data keyword.
tplot_names

print, "cleanup state"
del_data, 'th?_state*'

tplot_names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print, "-"
print, "- next: load data directly from L2 CDF"
print, "--> enter .c"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stop

; get the dsl data, and add a _l2 suffix
thm_load_fgm, probe='a', suffix='_l2', level = 'l2'
tplot_names

; load data in all coordinate systems availabe in L2 CDF
thm_load_fgm, probe='a', coord='*', level = 'l2'
tplot_names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
print, "-"
print, "- note that the original crib still works, with one change:"
print, "- new keyword to override new default behavior of thm_load_fgm: type='raw'"
print, "--> enter .c"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
stop

thm_load_state,/get_support_data,probe='a'
thm_load_fgm,level=1,probe=['a'],/get_support_data,type='raw'

; file names for real calibration files are: th[a-e]_fgmcal.txt
; they can be found in the 'master' file directory of the data tree.
cal_relpathname = 'tha/l1/fgm/0000/tha_fgmcal.txt'
cal_file = file_retrieve(cal_relpathname, _extra=!themis)

thm_cal_fgm,'tha_fgl','tha_fgl_hed','tha_fgl_ssl',cal_file

tplot_options, 'title', 'THEMIS FGM Examples'
tplot, ['tha_fgl', 'tha_fgl_ssl']

; Interface to ssl2dsl has changed: all arguments now specified by
; keywords instead of positional parameters, new arguments for
; using spinmodel routines.

ssl2dsl,name_input='tha_fgl_ssl',name_output='tha_fgl_dsl',spinmodel_ptr=spinmodel_get_ptr('a')

dsl2gse,'tha_fgl_dsl','tha_state_spinras','tha_state_spindec','tha_fgl_gse'

cotrans,'tha_fgl_gse','tha_fgl_gsm',/GSE2GSM

tplot, ['tha_fgl', 'tha_fgl_ssl', 'tha_fgl_dsl', 'tha_fgl_gse','tha_fgl_gsm']


end

