;+
;PROCEDURE:  istp_init
;PURPOSE:    Initializes system variables for ISTP data.  Can be called from idl_startup to set
;            custom locations.
;

;
;HISTORY
; Written by Davin Larson
;
;$LastChangedBy: davin-win $
;$LastChangedDate: 2008-08-05 11:22:24 -0700 (Tue, 05 Aug 2008) $
;$LastChangedRevision: 3355 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/missions/wind/istp_init.pro $
;-
pro istp_init, reset=reset  ;, local_data_dir=local_data_dir, remote_data_dir=remote_data_dir

defsysv,'!istp',exists=exists
if not keyword_set(exists) then begin
   defsysv,'!istp',  file_retrieve(/structure_format)
endif

if keyword_set(reset) then !istp.init=0

if !istp.init ne 0 then return

!istp = file_retrieve(/structure_format)

!istp.local_data_dir  = root_data_dir() + 'istp/'
!istp.remote_data_dir = 'http://cdaweb.gsfc.nasa.gov/istp_public/data/'
!istp.min_age_limit = 900    ; Don't check for new files if local file is less than 900 seconds old.
!istp.use_wget= getenv('username') eq 'davin'

;if keyword_set(local_data_dir) then  $
;   !istp.local_data_dir = local_data_dir

if file_test(!istp.local_data_dir+'.master') then !istp.no_download=1  ; Local directory IS the master directory

; To change default settings; create a new procedure:  istp_config.pro
libs,'istp_config',routine=name
if keyword_set(name) then call_procedure,name

!istp.init = 1

printdat,/values,!istp,varname='!istp

end


