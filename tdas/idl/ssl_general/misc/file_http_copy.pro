;+
;  WARNING: the interface to this routine is not yet solidified. Use the wrapper routine:
;  file_retrieve instead. This routine is still under development.
;
; NAME:
;    file_http_copy
;
; PURPOSE:
;    Downloads file(s) from http servers.
;    Also performs Searches without download.
;    Copies the file to a user specified local directory.
;    By default, files are only downloaded if the remote file is newer than
;    the local file (based on mtime) or if the files differ in size.
;    This routine is intended for use with simple HTTP file servers.
;    Wildcard matching and recursive file searching can be used as well.
;
; CALLING SEQUENCE: There are two methods:
; Method 1:
;    FILE_HTTP_COPY, pathnames, SERVERDIR=serverdir, LOCALDIR=localdir
;    where:
;      pathnames = (input string(s), scaler or array)  Relative path name of file to download.;
;      serverdir = (scaler input string)  Root name of source URL, must
;                                         begin with: 'http://' and end with '/'
;      localdir  = (scaler input string)  Root name of local directory, typically
;                                         ends with '/'
;    Note:   The source is at:    serverdir + pathnames
;            The destination is:  localdir + pathnames
; Method 2:
;    FILE_HTTP_COPY, URL
;       URL = full URL(S) of source file
;       Directory structure is not retained with this procedure
;
; Example:
;    FILE_HTTP_COPY, 'ssl_general/misc/file_http_copy.pro',  $
;              SERVERDIR='http://themis.ssl.berkeley.edu/data/themis/socware/bleeding_edge/idl/' $
;              localdir = 'myidl/'
;
;    Note: Unix style directory separaters '/' should be used throughout. This convention will still
;          work with WINDOWS.
;
; Alternate calling sequence:
;    FILE_HTTP_COPY,URL
;        where URL is an input (string) such as:  URL = '
;
; INPUTS:
;      URL - scalar or array string giving a fully qualified url
;
; OPTIONAL KEYWORDS:
;     NO_CLOBBER:   (0/1) Set this keyword to prevent overwriting local files.
;     IGNORE_FILESIZE: (0/1) Set this keyword to ignore file size when
;           evaluating need to download.
;     NO_DOWNLOAD:  (0/1) Set this keyword to prevent file downloads (url_info
;           is still returned)
;     URL_INFO=url_info: (output) Named variable that returns information about
;           remote file such as modification time and file size as determined
;           from the HTML header. A zero is returned if the remote file is
;           invalid.
;     FILE_MODE= file_mode:     If non-zero, sets the permissions for downloaded files.
;     DIR_MODE = dir_mode:      Sets permissions for newly created directories
;                            (Useful for shared directories)
;     ASCII_MODE:  (0/1)   When set to 1 it forces files to be downloaded as ascii text files (converts CR/LF)
;                          Setting this keyword will force ignore_filesize keyword to be set as well because
;                          files will be of different sizes typically.
;     VERBOSE:      (input; integer) Set level of verboseness:   Uses "DPRINT"
;           0-nearly silent;  2-typical messages;  4: debugging info
;
;
; Examples:
;   ;Download most recent version of this file to current directory:
;   FILE_HTTP_COPY,'http://themis.ssl.berkeley.edu/data/themis/socware/bleeding_edge/idl/ssl_general/misc/file_http_copy.pro'
;
; OPTIONAL INPUT KEYWORD PARAMETERS:
;       PATHNAME = pathname   ; pathname is the filename to be created.
;                If the directory does not exist then it will be created.
;                If PATHNAME does not exist then the original filename is used
;                and placed in the current directory.
;;
; RESTRICTIONS:
;
;     PROXY: If you are behind a firewall and have to access the net through a
;         Web proxy,  set the environment variable 'http_proxy' to point to
;         your proxy server and port, e.g.
;         setenv,  'http_proxy=http://web-proxy.mpia-hd.mpg.de:3128'
;
;               The URL *MUST* begin with "http://".
;
; PROCEDURE:
;     Open a socket to the webserver and download the header.
;
; EXPLANATION:
;     FILE_HTTP_COPY can access http servers - even from behind a firewall -
;     and perform simple downloads. Currently,
;     Requires IDL V5.4 or later on Unix or Windows, V5.6 on
;     Macintosh
;
; EXAMPLE:
;      IDL> FILE_HTTP_COPY,'http://themis.ssl.berkeley.edu/themisdata/thg/l1/asi/whit/2006/thg_l1_asf_whit_2006010103_v01.cdf'
;      IDL> PRINTDAT, file_info('thg_l1_asf_whit_2006010103_v01.cdf')
;      or
;
;
; MINIMUM IDL VERSION:
;     V5.4  (uses SOCKET)
; MODIFICATION HISTORY:
;   Original version:  WEBGET()
;     Written by M. Feldt, Heidelberg, Oct 2001 <mfeldt@mpia.de>
;     Use /swap_if_little_endian keyword to SOCKET  W. Landsman August 2002
;     Less restrictive search on Content-Type   W. Landsman   April 2003
;     Modified to work with FIRST image server-  A. Barth, Nov 2006
;   FILE_HTTP_COPY:   New version created by D Larson:  March 2007.
;     Checks last modification time of remote file to determine need for download
;     Checks size of remote file to determine need to download
;     Very heavily modified from WEBGET():
;   May/June 2007 - Modified to allow file globbing (wildcards).
;   July 2007   - Modified to return remote file info  (without download)
;   July 2007   - Modified to allow recursive searches.
;   August 2007 - Added file_mode keyword.
;   April  2008 - Added dir_mode keyword
;   Sep 2009    - Fixed user-agent
;
; $LastChangedBy: davin-win $
; $LastChangedDate: 2009-09-25 17:35:21 -0700 (Fri, 25 Sep 2009) $
; $LastChangedRevision: 6782 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_21/misc/file_http_copy.pro $
;-



pro extract_html_links,s,links, $   ; input: string  ;    output: links appended to array
    relative=relative,$   ; Set to return only relative links
    normal=normal         ; Set to return only normal links (without ? or *)

;compile_opt  idl2,hidden

      p0 = strpos(strlowcase(s),'<a href="')
      if p0 ge 0 then begin
         p1 = strpos(s,'">',p0)
         if p1 ge p0+9 then begin
            link = strmid(s,p0+9,p1-p0-9)
            bad = strlen(link) eq 0
            if keyword_set(relative) then bad = (strpos(link,'?') ge 0) or bad
            if keyword_set(relative) then bad = (strpos(link,'*') ge 0) or bad
            if keyword_set(relative) then bad = (strpos(link,'/') eq 0) or bad   ; remove absolute links (which start with '/')
            if not bad then links = [links,link]
         endif
      endif

end


;FUNCTION file_extract_html_links(filename,count)
;PURPOSE:  returns  links within an html file
;INPUT:  filename: (string) valid filename
;OUTPUT:  count:  number of links found
function file_extract_html_links,filename,count,verbose=verbose   ; Links with '*' or '?' or leading '/' are removed.
   count=0                                   ; this should only return the relative links.
   on_ioerror, badfile
   openr,lun,filename,/get_lun
   s=''
   links = ''
   while not eof(lun) do begin
      readf,lun,s
      extract_html_links,s,links,/relative,/normal
   endwhile
   free_lun,lun
   bad = strlen(links) eq 0
   w = where(bad eq 0,count)
   return,count gt 0 ? links[w] : ''
   badfile:
   dprint,dlevel=1,verbose=verbose,'Bad file: '+filename
   return,''
end


function file_http_header_element,header,name
  res = strcmp(header,name,strlen(name),/fold_case)
  g = where(res, Ng)
  if Ng GT 0 then return,strmid(header[g[0]],strlen(name)+1)
  return,''
end




pro file_http_header_info,  Header, hi, verbose=verbose
;;
;; MIME type recognition
;

;  hi.url = url
  if strmid(hi.url,0,1,/reverse_offset) eq '/' then hi.directory=1
  hi.ltime = systime(1)
  if not keyword_set(header) then return  ;,hi

  hi.status_str = header[0]
  header0 = strsplit(/extract,header[0],' ')
  hi.status_code = fix( header0[1] )

  ; get server time (date)
  date = file_http_header_element(header,'Date:')
  hi.atime = str2time(date, informat = 'DMYhms')
  hi.clock_offset = hi.atime - hi.ltime
  dprint,dlevel=6,verbose=verbose,'date=',date

  ; Look for successful return
  pos = strpos(strupcase(header[0]),'200 OK')
  hi.exists = hi.status_code eq 200 || hi.status_code eq 304
;  if hi.exists eq 0 then return

  hi.class = 'text'
  hi.type =  'simple'               ; in case no information found...
  hi.Content_Type = file_http_header_element(header,'Content-Type:')
  if keyword_set(hi.Content_Type) then begin
       hi.Class = (strsplit(hi.Content_Type, '/', /extract))[0]
       hi.Type = (strsplit(hi.Content_Type, '/', /extract))[1]
  ENDIF

    ; get file modification time
  last_modified = file_http_header_element(header,'Last-Modified:')
  hi.mtime = keyword_set(last_modified) ? str2time(last_modified, informat = 'DMYhms') : systime(1)
  dprint,dlevel=6,verbose=verbose,'last_modified=',last_modified

  ; Try to determine length
  len = file_http_header_element(header,'Content-Length:')
  if keyword_set(len) then  hi.size = long(len)   else hi.size = -1

  return   ;,hi
END




PRO file_http_copy, pathnames, newpathnames, $
     recurse_limit=recurse_limit, $
     verbose=verbose, $            ; input:  (integer)  Set level of verbose output.  (2 is typical)
     serverdir=serverdir,  $       ; input:  (string) URL of source files: ie:  'http://themis.ssl.berkeley.edu/data/themis/'      ;trailing '/' is required
     localdir=localdir, $          ; input:  (string) destination directory i.e.:  'e:/data/themis/'        ;trailing '/' is required
     localnames=localname, $       ; output:  Downloaded filenames are returned in this variable
     file_mode=file_mode,  $       ; input: if non-zero, file permissions are set to this value. (use '666'o for shared files.)
     dir_mode=dir_mode,   $        ; input:   defines directory permissions for newly created directories  (use '777'o for shared directories)
     last_version=last_version, $
     min_age_limit=min_age_limit, $
     user_agent=user_agent,   $    ; input string: Used to define user_agent in HTTP message.
     preserve_mtime=preserve_mtime,$
     restore_mtime=restore_mtime, $
     if_modified_since = if_modified_since, $
     ascii_mode = ascii_mode, $    ; input  (0/1)  Set this keyword to force downloaded files to be downloaded as ascii Text. (converts CR/LFs)
     no_globbing=no_globbing,  $
     no_clobber=no_clobber, $      ; input: (0/1)  set keyword to prevent overwriting existing files. (url_info is still returned however)
     no_update=no_update,   $      ; input: (0/1)  set keyword to prevent contacting remote server if file already exists. Ignored if globbing is used. (NOT YET OPERATIONAL!)
     no_download=no_download, $    ; input: (0/1)  set keyword to prevent downloading files, Useful to obtain url_info only.
     ignore_filesize=ignore_filesize, $    ; input: (0/1)  if set then file size is ignored when evaluating need to download.
     ignore_filedate=ignore_filedate, $    ; NOT YET OPERATIONAL! input: (0/1)  if set then file date is ignored when evaluating need to download.
     url_info=url_info_s,  $         ; output:  structure containing URL info obtained from the HTTP Header.
     progobj=progobj, $            ; This keyword is experimental - please don't count on it
     links=links2, $               ; Output: links are returned in this variable if the file is an html file
     error = error
  ;;
  ;;
  ;; sockets supported in unix & windows since V5.4, Macintosh since V5.6
tstart = systime(1)
;  if n_elements(verbose) eq 1 then dprint,setdebug=verbose,getdebug=last_dbg

dprint,dlevel=4,verbose=verbose,'Start; $Id: file_http_copy.pro 6782 2009-09-26 00:35:21Z davin-win $'
url_info_s = 0
if not keyword_set(localdir) then localdir = ''
if not keyword_set(serverdir) then serverdir = ''

for pni=0L,n_elements(pathnames)-1 do begin
  localname=''
  links2 = ''

  url_info = {http_info, $
    url:'',  $            ; Full url of file
    io_error: 0,  $
    localname:'', $       ; local file name
    status_str:'', $
    status_code:0,  $
    content_type:'', $
    type:'', $            ; type of file
    class:'', $           ; Class
    exists: 0b,  $
    directory: 0b, $
    ltime: 0ll, $         ; Time when procedure was run
    atime: 0ll, $         ; server time at time of last access
    mtime: 0ll, $         ; last mod time of file
    clock_offset: 0ll, $   ; difference between server time and local time
    size:  0ll $
  }
;url_info = 0

  if keyword_set(serverdir) then begin
      pathname = pathnames[pni]
      url = serverdir+pathname
  endif else begin
      url = pathnames[pni]
      pathname = file_basename(url)
      if strmid(url,0,1,/reverse_offset) eq '/' then pathname += '/'   ;add the '/' back on for directories
  endelse

  cgi_bin = strpos(url,'cgi-bin') gt 0

  if keyword_set(newpathnames) then begin
      no_globbing=1
      newpathname = newpathnames[pni]
  endif else newpathname = pathname

  if cgi_bin  && n_elements(no_globbing) eq 0 then no_globbing=1

  url_info.url = url
  url_info.ltime = systime(1)

  dprint,dlevel=6,verbose=verbose,/phelp,serverdir
  dprint,dlevel=6,verbose=verbose,/phelp,localdir
  dprint,dlevel=6,verbose=verbose,/phelp,pathname
  dprint,dlevel=6,verbose=verbose,/phelp,newpathname
  dprint,dlevel=6,verbose=verbose,/phelp,url
  indexfilename =  '.remote-index.html'


  globpos = min( uint( [strpos(pathname,'*'),strpos(pathname,'?'),strpos(pathname,'['),strpos(pathname,']')] ) )
  if (~ keyword_set(no_globbing)) && globpos le 1000 then begin   ; Look for globbed  ([*?]) filenames
     dprint,dlevel=4,verbose=verbose,'Warning! Using Globbing!'
     slash='/'
     slashpos1 = strpos(pathname,slash,globpos,/reverse_search)
     sub_pathname = strmid(pathname,0,slashpos1+1)
     dprint,dlevel=5,verbose=verbose,/phelp,sub_pathname
     ; First get directory listing and extract links:
     file_http_copy,sub_pathname,serverdir=serverdir,localdir=localdir,url_info=index $
         ,min_age_limit=min_age_limit,verbose=verbose,file_mode=file_mode,dir_mode=dir_mode,if_modified_since=if_modified_since $
         ,no_update=no_update, links=links, user_agent=user_agent,preserve_mtime=preserve_mtime, restore_mtime=restore_mtime
     dprint,dlevel=4,verbose=verbose,/phelp,links
     slashpos2 = strpos(pathname,slash,globpos)
     if slashpos2 eq -1 then slashpos2 = strlen(pathname)  ; special case for non-directories  (files)
     sup_pathname = strmid(pathname,0,slashpos2+1)
     end_pathname = strmid(pathname,slashpos2+1)
     w = where(strmatch(sub_pathname+links,sup_pathname),nlinks)
     if nlinks gt 0 then begin
         rec_pathnames = sub_pathname + links[w] + end_pathname
         dprint,dlevel=5,verbose=verbose,/phelp,sup_pathname
         dprint,dlevel=5,verbose=verbose,/phelp,end_pathname
         dprint,dlevel=5,verbose=verbose,/phelp,rec_pathnames
         if keyword_set(last_version) then i0 = nlinks-1  else i0=0L
         for i=i0,nlinks-1 do begin
             dprint,dlevel=4,verbose=verbose,'Retrieve link#'+strtrim(i+1,2),' of ',strtrim(nlinks,2),': ', rec_pathnames[i]
             ; Recursively get files:
             file_http_copy,rec_pathnames[i],serverdir=serverdir,localdir=localdir  $
                       , verbose=verbose,file_mode=file_mode,dir_mode=dir_mode, ascii_mode=ascii_mode  $
                       , min_age_limit=min_age_limit, last_version=last_version, url_info=ui $
                       , no_download=no_download, no_clobber=no_clobber, no_update=no_update $
                       , ignore_filesize=ignore_filesize,user_agent=user_agent,if_modified_since=if_modified_since $
                       , preserve_mtime=preserve_mtime, restore_mtime=restore_mtime
             dprint,dlevel=5,verbose=verbose,/phelp,lns
             if not keyword_set(ui)  then message,'URL info error'
             w = where(ui.exists ne 0,nw)
             if nw ne 0 then uis  = keyword_set(uis)  ?  [uis,ui[w]]   : ui[w]  ; only include existing files
             dprint,dlevel=5,verbose=verbose,/phelp,localname
         endfor
         if keyword_set(uis) then url_info = uis
     endif else begin
         dprint,dlevel=3,verbose=verbose,'No files found matching: '+sup_pathname
     endelse
     goto, final
  endif             ;  End of globbed filenames

  ; Begin normal file downloads
  localname = localdir + newpathname

  if strmid(url,0,1,/reverse_offset) eq '/' then begin    ; Directories
     url_info.directory = 1
     localname = localname + indexfilename
  endif
  lcl = file_info(localname)

  if keyword_set(no_update) and lcl.exists then begin
      dprint,dlevel=2,verbose=verbose,'Warning: Updates to existing file: "',lcl.name,'" are not being checked!'
      url_info.localname = localname
      url_info.exists = -1   ; existence is not known!
      if arg_present(links2) then links2 = file_extract_html_links(localname)
      goto, final
  endif

  if lcl.exists eq 1 and lcl.write eq 0 then begin
      dprint,dlevel=2,verbose=verbose,'Local file: '+lcl.name+ ' exists and is write protected. Skipping.'
      url_info.localname = localname
      url_info.exists = -1   ; existence is not known!
      if arg_present(links2) then links2 = file_extract_html_links(localname)
      goto, final
  endif

;Warning: The file times (mtime,ctime,atime) can be incorrect (with Windows) if the user has (recently) changed the time zone the computer resides in.
;This can lead to unexpected results.
  if tstart-lcl.mtime lt (keyword_set(min_age_limit) ? min_age_limit : 0) then begin
      dprint,dlevel=3,verbose=verbose,'Found recent file: "'+localname+'" (assumed valid)'
      ;url_info.ltime = systime(1)
      url_info.localname = localname
      url_info.exists = 1
      if arg_present(links2) then links2 = file_extract_html_links(localname)
      goto, final
  endif

  ;;
  ;; open the connection and request the file
  ;;
  read_timeout = 30
  connect_timeout = 10
  t_connect = systime(1)

  if n_elements(user_agent) eq 0 then user_agent =  'FILE_HTTP_COPY IDL'+!version.release + ' ' + !VERSION.OS + '/' + !VERSION.ARCH

  stack = scope_traceback(/structure)
  referrer = stack.routine + string(stack.line,format='("(",i0,")")')
  referrer = strjoin(referrer,':')

;  filemodtime = lcl.mtime

  Proxy = getenv('http_proxy')
  IF Proxy NE '' THEN BEGIN    ; sort out proxy name
      dprint,dlevel=1,verbose=verbose,'Using Proxy: ',proxy
      Server = strmid(proxy, 7 )
      Purl = url
  ENDIF ELSE BEGIN    ; Without proxy
      slash1 = StrPos(strmid(url, 7, StrLen(url)), '/')
      Server = StrMid(url, 7, slash1 )
      purl = strmid(url,slash1+7, StrLen(url))
  ENDELSE

      lastcolon = strpos(server,':', /reverse_search)
      if lastcolon gt 1 then begin
         port = fix(strmid(server,lastcolon+1)  )
         server = strmid(server,0,lastcolon)
      endif else port = 80

      dprint,dlevel=4,verbose=verbose,'Opening server: ',server, ' on Port: ',port
      if not keyword_set(server) then dprint,dlevel=0,verbose=verbose,'Bad server'
      socket, unit, Server,  Port, /get_lun,/swap_if_little_endian,error=error,$
                 read_timeout=read_timeout,connect_timeout=connect_timeout
      if error ne 0 then begin
          If(n_elements(unit) Gt 0) Then free_lun, unit  ;jmm, 19-jun-2007 for cases where unit is undefined
          dprint,dlevel=0,verbose=verbose,!error_state.msg
          goto, final2
      endif
      dprint,dlevel=4,verbose=verbose,'Purl= ',purl
      printf, unit, 'GET '+purl +  ' HTTP/1.0'
      if keyword_set(user_agent) then begin
         printf, unit, 'User-Agent: ',user_agent
         dprint,dlevel=4,verbose=verbose,'User Agent: ',user_agent
      endif
      if size(/type,referrer) eq 7 then begin
         printf, unit,  'Referer: ',referrer
         dprint,dlevel=4,verbose=verbose,'Referer: ',referrer
      endif
      if keyword_set(if_modified_since) then begin
         filemodtime = if_modified_since lt 2 ? lcl.mtime : if_modified_since
         printf, unit, 'If-Modified-Since: ',time_string(filemodtime,tformat='DOW, DD MTH YYYY hh:mm:ss GMT')
         dprint,dlevel=4,verbose=verbose,'If-Modified-Since: ',time_string(filemodtime,tformat='DOW, DD MTH YYYY hh:mm:ss GMT')
      endif
      printf, unit, ''

  LinesRead = 0
  text = 'XXX'
  ;;
  ;; now read the header
  ;;
On_IOERROR, done

  Header = strarr(256)
  WHILE  text NE '' do begin
      readf, unit, text
      Header[LinesRead] = text
      LinesRead = LinesRead+1
      IF LinesRead MOD 256 EQ 0 THEN $
        Header=[Header, StrArr(256)]
  ENDWHILE
DONE: On_IOERROR, NULL
  ;;
  if LinesRead EQ 0 then begin
     free_lun, unit
     url_info.io_error = 1
     dprint,dlevel=0,verbose=verbose,!error_state.msg
     goto, final
  endif

  Header = Header[0:LinesRead-1]

  url_info.localname = localname
  file_http_header_info,header,url_info,verbose=verbose

  dprint,dlevel=4,verbose=verbose,'Server ',server,' Connect time= ',systime(1)-t_connect
  dprint,dlevel=6,verbose=verbose,'Header= ',transpose(header)
  dprint,dlevel=6,verbose=verbose,phelp=2,url_info

  if url_info.status_code eq 302 then begin   ; Redirection
      location = file_http_header_element(header,'Location:')
      dprint,dlevel=1,verbose=verbose,'Redirection to: ',location
      if keyword_set(location) then begin
          file_http_copy,location,keyword_set(newpathnames) ? newpathname : '', $
               localdir=localdir,verbose=verbose, $
               url_info=url_info,file_mode=file_mode,dir_mode=dir_mode, ascii_mode=ascii_mode, $
               user_agent=user_agent,preserve_mtime=preserve_mtime,restore_mtime=restore_mtime,if_modified_since=if_modified_since
          goto, close_server
      endif
  endif

  if abs(url_info.clock_offset) gt 30 then $
      dprint,dlevel=1,verbose=verbose,'Warning! Remote and local clocks differ by:',url_info.clock_offset,' Seconds

  if url_info.status_code eq 304 then begin   ;  Not modified since
      dprint,dlevel=2,verbose=verbose,'Local file is current'
      goto,  close_server

  endif

  if url_info.exists then begin

      ; Determine if download is needed

         tdiff = (url_info.mtime - lcl.mtime)  ; seconds old
         MB = 2.^20
         if lcl.exists then begin
             download_file = 0
             dprint,verbose=verbose,dlevel=4,'tdiff=',tdiff,' sec'
             if tdiff gt 0  then begin
                if keyword_set(no_clobber) then dprint,dlevel=1,verbose=verbose, format="('Warning!  ',f0.1,' day old local file: ',a  )", tdiff/24d/3600d, localname
                download_file = 1
             endif
             if tdiff lt 0 and keyword_set(restore_mtime) then  begin
                file_touch,exists=texists
                if texists then dprint,dlevel=3,verbose=verbose,'FILE TIME MISMATCH!   ',lcl.name
                if keyword_set(restore_mtime) and keyword_set(preserve_mtime) and lcl.size eq url_info.size then file_touch,lcl.name,url_info.mtime,/mtime,/no_create,verbose=verbose
             endif
             if lcl.size ne url_info.size && not keyword_set(ascii_mode) then begin
                if keyword_set(no_clobber) then $
                    dprint,dlevel=1,verbose=verbose,url_info.size/mb,lcl.size/mb, file_basename(localname), format='("Warning! Different file sizes: Remote=",f0.3," MB, Local=",f0.3," MB file: ",a)'
                if not keyword_set(ignore_filesize) then download_file = 1
             endif
             if keyword_set(no_clobber) then download_file=0
         endif else begin     ; endof lcl.exists
             download_file = 1
             dprint,dlevel=3,verbose=verbose,format="('Found remote (',f0.3,' MB) file: ""',a,'""')",url_info.size/mb,url
         endelse

         if keyword_set(no_download) then  download_file = 0

         if download_file then begin    ;  begin file download
             dirname = file_dirname(localname)
             file_mkdir2,dirname,mode = dir_mode,dlevel=2,verbose=verbose
             On_IOERROR, file_error2
             if file_test(localname,/regular,/write) then begin
                dprint,'Deleting old file: ',localname,dlevel=3,verbose=verbose
                file_delete,localname,/allow_nonexistent
;                file_move,localname,localname+'.old'
;                printdat,file_info(localname)
;                file_delete,localname+'.old'            ; delete old file to reset old creation time.
;                printdat,file_info(localname+'.old')
;                wait,30   ; an extremeley long wait is needed to make this work on windows.
;                dprint
             endif
             openw, wunit, localname, /get_lun
             ts = systime(1)
             t0 = ts
             if keyword_set(ascii_mode) || url_info.size lt 0 || strmid(url_info.type,0,4) eq 'html'  then begin         ; download text file (typically these are directory listings
               dprint,dlevel=3,verbose=verbose,'Downloading "',localname,'" as a text file.'
               lines=0ul
               while  eof(unit) EQ 0 do begin
                 readf, unit, text
                 printf, wunit, text
                 if arg_present(links2) then extract_html_links,text,links2 ,/relative, /normal
                 dprint,dwait=10,dlevel=1,verbose=verbose,'Downloading "',localname,'"  Please wait ', lines++
               endwhile
               dprint,dlevel=2,verbose=verbose,'Downloaded ',strtrim(lines,2),' lines in ',string(systime(1)-ts,format='(f0.2)'),' seconds. File:',localname
               if n_elements(links2) gt 1 then links2 = links2[1:*]   ; get rid of first ''
             endif else begin                                                      ; download Non-text (binary) files
               maxb = 2l^20   ; 1 Megabyte default buffer size
               nb=0l
               b=0l
               while nb lt url_info.size do begin
                 buffsize = maxb  <  (url_info.size-nb)
                 aaa = bytarr(buffsize,/nozero)
                 readu, unit, aaa
                 writeu, wunit, aaa
                 nb += buffsize
                 t1 = systime(1)
                 dt = t1-t0
                 b += buffsize
                 percent = 100.*float(nb)/url_info.size
                 if (dt gt 5.) and (nb lt url_info.size) then begin   ; Wait 5 seconds between updates.
                    rate = b/mb/dt                             ; This will only display if the filesize (url_info.size) is greater than MAXB
                    eta = (url_info.size-nb)/mb/rate +t1 - tstart
                    messstr = string(format='("  ",f5.1," %  (",f0.1,"/",f0.1," secs)  @ ",f0.2," MB/s  File: ",a)', percent, t1-tstart,eta, rate,file_basename(localname) ,/print)
                    t0 = t1
                    b =0l
                    dprint,dlevel=1,verbose=verbose,messstr    &  wait,.01
                    if obj_valid(progobj)  then begin
                        progobj->update,percent,text=messstr
                        if progobj->checkcancel() then message,'Download cancelled by user',/ioerror
                    endif
                 endif
               endwhile
               t1 = systime(1)
               dt = t1 - tstart
               messstr = string(/print,format = "('Downloaded ',f0.3,' MBytes in ',f0.1,' secs @ ',f0.2,' MB/s  File: ""', a,'""' )",nb/mb,dt,nb/mb/dt,localname )
               dprint,dlevel=2,verbose=verbose,messstr
               if obj_valid(progobj)  then begin
                 progobj->update,percent,text=messstr
               endif
             endelse
             free_lun, wunit
             if keyword_set(file_mode) then file_chmod,localname,file_mode
             if keyword_set(preserve_mtime) then file_touch,localname,url_info.mtime,/mtime,/no_create,verbose=verbose
             if 0 then begin
                 file_error2:
                 beep
                 dprint,dlevel=0,verbose=verbose,'Error downloading file: "',url,'"'
                 error = !error_state.msg
                 dprint,dlevel=0,verbose=verbose,error
                 if obj_valid(progobj)  then begin
                    progobj->update,0.,text=error
                 endif
                 if keyword_set(wunit) then free_lun, wunit
;                 dprint,dlevel=0,verbose=verbose,'Deleting: "' + lcl.name +'"'
;                 file_delete,lcl.name     ; This is not desirable!!!
             endif
         endif else begin
             dprint,dlevel=3,verbose=verbose,'Local file: "' + localname + '" is current (Not downloaded)'
         endelse
  endif else begin
      dprint,dlevel=1,verbose=verbose,'Remote file not found! "'+ url + '"'
;      url_info = 0
  endelse

  close_server:
  free_lun, unit
  dprint,dlevel=5,verbose=verbose,'Closing server: ',server

  final:

  if keyword_set(recurse_limit) then begin    ; Recursive search for files.

     if keyword_set(index) then if index.localname ne localdir+indexfilename then links=''

     if not keyword_set(links) then begin   ; Get directory list
         file_http_copy,'',serverdir=serverdir,localdir=localdir, $
             min_age_limit=min_age_limit,verbose=verbose,no_update=no_update, $
             file_mode=file_mode,dir_mode=dir_mode,ascii_mode=1 , $
             url_info=index,links=links,user_agent=user_agent,if_modified_since=if_modified_since   ;No need to preserve mtime on dir listings ,preserve_mtime=preserve_mtime
     endif
     wdir = where(strpos(links,'/',0) gt 0,ndirs)
     for i=0,ndirs-1 do begin
         dir = links[wdir[i]]
         file_http_copy,pathname,recurse_limit=recurse_limit-1,serverdir=serverdir+dir,localdir=localdir+dir $
                       , verbose=verbose,file_mode=file_mode,dir_mode=dir_mode,ascii_mode=ascii_mode $
                       , min_age_limit=min_age_limit, last_version=last_version, url_info=ui $
                       , no_download=no_download, no_clobber=no_clobber,no_update=no_update  $
                       , ignore_filesize=ignore_filesize,user_agent=user_agent $
                       , preserve_mtime=preserve_mtime,retore_mtime=restore_mtime,if_modified_since=if_modified_since
         if not keyword_set(ui)  then message,'URL error'
         w = where(ui.exists ne 0,nw)
         if nw ne 0 then url_info  = keyword_set(url_info)  ?  [url_info,ui[w]]   : ui[w]  ; only include existing files
     endfor
  endif

;  if keyword_set(url_info) then localname=url_info.localname else localname=''

  final2:
  if keyword_set(url_info_s) and keyword_set(url_info) then $
       url_info_s=[url_info_s,url_info] else url_info_s=url_info

endfor

if keyword_set(url_info_s) then localname=url_info_s.localname else localname=''
dprint,dlevel=5,verbose=verbose,'Done'
;if n_elements(verbose) ne 0 then dprint,setdebug=last_dbg           ; Reset previous debug level.
return

END
