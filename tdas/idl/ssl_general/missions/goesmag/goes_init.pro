;+
;PROCEDURE:  goes_init
;PURPOSE:    Initializes system variables for GOES.  Can be called from idl_startup to set
;            custom locations.
;
; The system variable !GOES is defined here.  The elements of this structure
; are the same as for !THEMIS.
;
;$LastChangedBy: davin-win $
;$LastChangedDate: 2009-05-22 12:08:39 -0700 (Fri, 22 May 2009) $
;$LastChangedRevision: 5947 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/missions/goesmag/goes_init.pro $
;-
pro goes_init,reset=reset

defsysv,'!goes',exists=exists
if ~keyword_set(exists) || keyword_set(reset) then begin
   defsysv,'!goes', file_retrieve(/structure_format)
   ftest = goes_read_config()
   if(size(ftest, /type) Eq 8) && ~keyword_set(reset) Then Begin
       print, 'Loading saved GOES config.'
       !goes.local_data_dir = ftest.local_data_dir
       !goes.remote_data_dir = ftest.remote_data_dir
       !goes.no_download = ftest.no_download
       !goes.no_update = ftest.no_update
       !goes.downloadonly = ftest.downloadonly
       !goes.verbose = ftest.verbose

    endif else begin
       if keyword_set(reset) then begin
         print,'Resetting GOES to default configuration'
       endif else begin
         print,'No GOES config found...creating default configuration'
       endelse
       defsysv,'!goes', file_retrieve(/structure_format)
       !goes.remote_data_dir = 'http://themis.ssl.berkeley.edu/data/goes/'
       !goes.local_data_dir = root_data_dir() + 'goes/'
       !goes.no_download = file_test(!goes.local_data_dir + '.goes_master',/regular)
       print,'Saving default GOES config...'
       goes_write_config
    endelse
    printdat,/values,!goes,varname='!goes'   ;,/pgmtrace
endif

return
end
