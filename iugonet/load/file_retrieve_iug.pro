;+
; FUNCTION file_retrieve_iug(pathnames,local_data_dir=local_data_dir)
;
; PURPOSE:
;    This procedure is the modified version of file_retrieve in tdas_6_00 for IUGONET data.
;    It calls file_http_copy_iug inside.
;
; Modified by Y. Tanaka, May 9, 2011.
;-;-
function file_retrieve_iug,pathnames, newpathnames, structure_format=structure_format,  $
    use_wget=use_wget, nowait=nowait, $
    local_data_dir=local_data_dir,remote_data_dir=remote_data_dir, $
    min_age_limit=min_age_limit , $
    last_version = last_version , $
    file_mode = file_mode,  $
    dir_mode = dir_mode,   $
    user_agent=user_agent,   $
    preserve_mtime=preserve_mtime,  $
    ascii_mode=ascii_mode,   $
    no_download=no_download,no_server=no_server, $
    no_update=no_update, $
    no_clobber=no_clobber, ignore_filesize=ignore_filesize, $
    verbose=verbose,progress=progress,progobj=progobj

dprint,dlevel=4,verbose=verbose,'Start; $Id: file_retrieve_iug.pro 8174 2011-02-11 21:41:51Z davin-win $'
if keyword_set(structure_format) then begin
   user_agent =  'FILE_RETRIEVE_IUG: IDL'+!version.release + ' ' + !VERSION.OS + '/' + !VERSION.ARCH+ ' (' + (getenv('USER') ? getenv('USER') : getenv('USERNAME'))+')'
   str= {   $
      retrieve_struct,       $
      init:0,                $
      local_data_dir:root_data_dir(),  $ ;getenv('ROOT_DATA_DIR'),
      remote_data_dir:'',    $
      progress: 1    ,       $    ; Currently unused keyword   (progress is printed by default)
      user_agent:user_agent, $    ; User agent text to be sent to web server.
      file_mode:'666'o  ,    $    ; permissions for new files. (if non-zero)
      dir_mode: '777'o  ,    $    ; permissions for newly created directories.
      preserve_mtime: 1 ,    $    ; Set file modification to same as on file on server  (uses file_touch executable)
      progobj: obj_new(),    $    ; Experimental option for a progress bar widget.  (please ignore for now)
      min_age_limit: 30L   , $    ;   Files younger than this age (in seconds) are assumed current (avoids the need to check server)
      no_server:0     ,      $    ; Set to 1 to prevent any contact with a remote server.
      no_download:0   ,      $    ; Identical to NO_SERVER keyword. Obsolete, but retained for backward compatibility.
      no_update:0     ,      $    ; Set to 1 to prevent contact to server if local file already exists. (this is similar to no_clobber)
      no_clobber:0    ,      $    ; Set to 1 to prevent existing files from being overwritten. (A warning message will be displayed if remote server has)
      ignore_filesize:0 ,    $    ; Set to 1 to ignore the remote/local file sizes when determining if updates are needed.
      ignore_filedate:0 ,    $    ; Not yet operational.
      downloadonly:0  ,      $    ; Set to 1 to only download files but not load files into memory.
      use_wget:0          ,   $   ; Experimental option (uses the routine WGET instead of file_http_copy_iug)
      nowait:0        ,      $    ; Used with wget to download files in the background.
      verbose:2              $
   }
   return, str
endif

if keyword_set(no_download) then no_server = no_download
;if not keyword_set(local_data_dir) then   local_data_dir = './'
;if not keyword_set(remote_data_dir) then   remote_data_dir = ''
vb = keyword_set(verbose) ? verbose : 0
if n_elements(progress) eq 0 then progress=1

;if keyword_set(progress) then begin
;    progobj = obj_new('progressbar')
;endif


;fullnames = filepath(root_dir=local_data_dir, pathnames)
fullnames = local_data_dir + pathnames   ; trailing '/' is required on local_data_dir
n0 = n_elements(fullnames)

if keyword_set(use_wget) and total(/preserv,strmatch(pathnames,'*[ \* \? \[ \] ]*') ) ne 0 then begin
     use_wget=0
     dprint,dlevel=1,verbose=verbose,'Warning! WGET can not be used with wildcards!'
endif


if keyword_set(remote_data_dir) and  not (keyword_set(no_server) or keyword_set(no_download)) then begin

  if keyword_set(use_wget) then $
     wget,serverdir=remote_data_dir,localdir=local_data_dir,pathname=pathnames,verbose=verbose ,nowait=nowait $
  else begin

     http = strmid(remote_data_dir,0,7) eq 'http://'
     If obj_valid(progobj) Then progobj -> update, 0.0, text = string(format="('Retrieving ',i0,' files from ',a)",n0,remote_data_dir) ;jmm, 15-may-2007
     for i = 0l,n0-1 do begin
         fn = fullnames[i]
         pn = pathnames[i]
         npn = keyword_set(newpathnames) ? newpathnames[i] : ''
;         if keyword_set(no_update) and file_test(fn,/regular) then continue
         if http then begin
             file_http_copy_iug,pn,npn,url_info=url_info,serverdir=remote_data_dir,localdir=local_data_dir,verbose=verbose, $
               no_clobber=no_clobber,no_update=no_update,ignore_filesize=ignore_filesize,progobj=progobj, $
               ascii_mode=ascii_mode, $
               user_agent=user_agent, $
               preserve_mtime = preserve_mtime, $
               file_mode=file_mode,dir_mode=dir_mode,last_version=last_version,min_age_limit=min_age_limit
             if url_info.io_error ne 0 then begin
               dprint, "File or URL i/o error detected.  See !error_state for more info"
               printdat,!error_state
               return,''
             endif
         endif  else begin
             file_copy2,serverdir=remote_data_dir,localdir=local_data_dir,pathname=pn,verbose=verbose,no_clobber=no_update
         endelse
     endfor

  endelse

endif

; The following bit of code should find the highest version number if globbing is used.

for i=0,n_elements(fullnames)-1 do begin
   ff = file_search(fullnames[i],count=c)
   case c of
   0:    dprint,dlevel=3,verbose=vb,'No matching file: "'+fullnames[i]+'"'
   1:    begin
           fullnames[i] = ff[0]
           dprint,dlevel=5,verbose=vb,'Found: "'+fullnames[i]+'"'
         end
   else: begin
           dprint,dlevel=2,verbose=vb,'Multiple matches found for: "'+fullnames[i]+'"  Using last version.'
           fullnames[i] = ff[n_elements(ff)-1]   ; Cluge to Use highest version number?
         end
   endcase
endfor

return,fullnames
end

