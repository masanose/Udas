;+
;PROCEDURE:  wind_init
;PURPOSE:    Initializes system variables for WIND.  Can be called from idl_startup to set
;            custom locations.
;
;HISTORY
; Written by Davin Larson
;
;$LastChangedBy: davin-win $
;$LastChangedDate: 2008-04-16 13:15:13 -0700 (Wed, 16 Apr 2008) $
;$LastChangedRevision: 2738 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/missions/wind/wind_init.pro $
;-
pro wind_init, reset=reset  ;, local_data_dir=local_data_dir, remote_data_dir=remote_data_dir

defsysv,'!wind',exists=exists
if not keyword_set(exists) then begin
   defsysv,'!wind',  file_retrieve(/structure_format)
endif

if keyword_set(reset) then !wind.init=0

if !wind.init ne 0 then return

!wind = file_retrieve(/structure_format)
!wind.local_data_dir = root_data_dir()
!wind.remote_data_dir = 'http://sprg.ssl.berkeley.edu/data/

if file_test(!wind.local_data_dir+'wind/.master') then !wind.no_download=1  ; Local directory IS the master directory

libs,'wind_config',routine=name
if keyword_set(name) then call_procedure,name

!wind.init = 1

printdat,/values,!wind,varname='!wind

end

