;+
;Procedure  FILE_OPEN,type,name
;Purpose:  wrapper for  OPENW, OPENU, OPENR,  FILE_MKDIR
;type must be one of:  'w', 'u', 'r',  'd'  (write, update, read, directory)
;   This procedure performs the functions of creating/opening files and creating directories.
;   It has several useful features which the regular procedures do not have:
;  1)  Non-existant directories are created automatically (with optional mode setting)
;  2)  If a non-existant file is opened for update, it is created with OPENW instead of producing an error.
;  3)  Files (or directories) can be tested to see if they can be created before actually opening (or creating)
;           them (use the TEST keyword). (The returned structure INFO will have the WRITE element set)
;
;Example 1:   Creating a file for write access:
;   FILE_OPEN,'w','/dir1/dir2/dir3/file.tmp',unit=u  ; Will create the director tree if it does not already exist.
;Example 2:   Creating new directories with defined modes:
;   FILE_OPEN,'d','/dir1/dir2/dir3', dir_mode="777 ,
;     All newly created directories will have the given permissions  (Octal 777)
;Example 3:
;   FILE_OPEN,'w','/dir1/dir2/dir3/file.tmp',/test,info=info  ;This will test if the file
;          can be created (with given file system permissions) without actually creating the file.
;          The returned info stucture can potentially have info.exists eq to 0 and info.write eq 1
;Example 4:
;-

pro file_open,type,name,unit=unit,createable=createable,info=fi,test_only=test_only,file_mode=file_mode,dir_mode=dir_mode,dlevel=dlevel,verbose=verbose,error_message=mss

fi = file_info(name)
dprint,dlevel=4,verbose=verbose,'"',type,'" ',name
mss = ''
create = ~keyword_set(test_only)
if create then unit=0
tp = strlowcase(strmid(type,0,1))
createable=fi.write

if fi.exists then begin
   if tp eq 'd' then begin
      if fi.directory then  return   ; directory already exists!
      createable = 0
      mss = name+' exists but is not a directory!'
      dprint,dlevel=dlevel,verbose=verbose,mss
      return
   endif
   if fi.directory then begin
      createable = 0
      mss = name+' is a directory, can not create as file.!'
      dprint,dlevel=dlevel,verbose=verbose,mss
      return
   endif
   if fi.regular && create then begin
      dprint,dlevel=dlevel,verbose=verbose,'Opening existing file: '+name
      case tp of
        'w': if fi.write then  openw,unit,name,/get_lun,_extra=ex
        'u': if fi.write then  openu,unit,name,/get_lun,_extra=ex
        'r': if fi.read  then  openr,unit,name,/get_lun,_extra=ex
        else:  dprint,'Invalid type: '+type
      endcase
      return
   endif
   return
endif

if tp eq 'r' then begin
   if ~create then return
   mss = 'No such file: '+name
   dprint,dlevel=dlevel,verbose=verbose,mss
   return
endif


parent_dir = file_dirname(name)   ; Create all parent directories (if they don't exist)
if parent_dir ne name then $
   file_open,'d',parent_dir,test_only=test_only,createable=d_writeable,dir_mode=dir_mode,info=di,error_message=mss $   ; create parent directory first, make sure it is writeable
else di={file_info}


if tp eq 'd' then begin       ; Create new directory
   if d_writeable then begin
     if create then begin
        mss1='Creating new directory: '+name
        dprint,dlevel=dlevel,verbose=verbose,mss1
        file_mkdir,name
        if keyword_set(dir_mode) then file_chmod,name,dir_mode
        fi= file_info(name)
     endif
   endif else begin
     if create then begin
        mss = 'Unable to create directory: '+name
        dprint,dlevel=dlevel,verbose=verbose,mss
     endif
   endelse
   createable = d_writeable
   return
endif

if tp eq 'u' or tp eq 'w' then begin    ; create new files
   if d_writeable && create then begin
      dprint,'Creating new file: '+name
      openw,unit,name,/get_lun,_extra=ex
      if n_elements(file_mode) ne 0 then file_chmod,name,file_mode
      fi = file_info(name)
      createable = fi.write
      return
   endif
   createable = d_writeable
   return
endif


mss='Invalid type: '+type
dprint,dlevel=dlevel,verbose=verbose,mss
return


end





; test_code:

;  file_delete,'/disks/data/misc/dir1/dir2',/recursive
unit=0
test=0
name = '/disks/data/misc/dir1/dir2/dir3/test.tmp'
file_open,'w',test=test,name,info=ui,creat=cr,unit=unit,error=mss,dir_mode='777'o , file_mode='666'o
printdat,unit,cr,ui,mss

help,/files
if keyword_set(unit) then free_lun,unit
end

