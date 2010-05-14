;+
; FILE_TOUCH
; Purpose:  Wrapper routine for the "touch" program that sets file modification and access times
; USAGE:
;    file_touch,'foo',systime(1)-300,/mtime   ; sets mod time to 5 minutes ago
; keywords:
;    MTIME    set only modification time
;    ATIME    set only access time
;    VERBOSE  sets VERBOSITY of messages (0: error messages only,  6: lots)
; Restrications:
;   Shell executable "touch" must be in path on local operating system.  This is common on unix systems.
;   Windows executable available from: http://sourceforge.net/projects/unxutils/
;   If the touch executable is not found then no action is taken.
;   Test for executable occurs only once.
;-


pro file_touch,file,time,mtime=mtime,atime=atime,no_create=no_create,toffset=toffset,exit_status=status,verbose=verbose,exists=exists

common file_touch_com, touch_init,touch_version

if ~keyword_set(touch_init)  then begin
    spawn,'touch --version',touch_version
    touch_init = 1
endif

if ~keyword_set(touch_version) then begin
    dprint,verbose=verbose,dlevel=touch_init-1 ,'Executable "touch" not found. Ingoring.'
    touch_init =4
    return
endif else dprint,verbose=verbose,dlevel=6,touch_version

if arg_present(exists) then begin
   exists = keyword_set(touch_version)
   return
endif

if size(/type,file) ne 7 then begin
    dprint,verbose=verbose,'filename required.'
    return
endif

commands = 'touch'
if keyword_set(mtime) then commands = [commands,'-m']
if keyword_set(atime) then commands = [commands,'-a']
if keyword_set(no_create) then commands = [commands,'-c']
if keyword_set(time) then begin
   if n_elements(toffset) eq 0 then toffset= ' +0'
   tstring = time_string(time[0], tformat= 'YYYY-MM-DD hh:mm:ss') + toffset
endif

if !version.os_family eq 'unix' then begin
   if keyword_set(tstring) then commands = [commands,'-d',tstring]
   commands = [commands,file]
   dprint,verbose=verbose,dlevel=4,commands
   spawn,commands ,/noshell,/stderr,output,exit_status=status
endif

if !version.os_family eq 'Windows' then begin
   if keyword_set(tstring) then tstring = '-d "' + tstring + '"' else tstring=''
   filestring = '"' + file + '"'
    command = strjoin([commands,tstring,filestring],' ')
    dprint,verbose=verbose,dlevel=4,command
    spawn,command ,/noshell,/stderr, /hide,output ,exit_status=status
endif

if keyword_set(output) then dprint,dlevel=1,verbose=verbose,output


end

