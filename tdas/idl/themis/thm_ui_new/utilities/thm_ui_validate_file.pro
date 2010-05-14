;+
;PRO:
;  thm_ui_validate_file
;
;PURPOSE:
;
;  Verifies file read/write permissions and file availability
;
;Inputs:
;  filename:name of the file
;
;Outputs:
;  statuscode: negative value indicates failure, 0 indicates success
;  statusmsg: a message to be returned in the event of an error
;
;Keywords:
;  write: validate for save
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-09-14 14:58:32 -0700 (Mon, 14 Sep 2009) $
;$LastChangedRevision: 6722 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/thm_ui_validate_file.pro $
;-


pro thm_ui_validate_file,filename=filename,statuscode=statuscode,statusmsg=statusmsg,write=write

catch,Error_status

if (Error_status NE 0) then begin
   statusmsg = !ERROR_STATE.MSG
   statuscode = -3
   catch,/cancel
   return
endif

statuscode = 0
statusmsg = ''

if ~is_string(filename) then begin
 statusmsg='"'+routine_name+'": Malformed filename' 
 statuscode=-1
 return
endif

filename = (expand_path(filename))[0]

traceback = scope_traceback(/structure)

routine_name = traceback[n_elements(traceback)-2].routine

tgt_dirname=file_dirname(filename)

fi=file_info(tgt_dirname)

if keyword_set(write) then begin
  if ~fi.exists then begin
     statusmsg=string(tgt_dirname,format='("'+routine_name+': Failed. Directory ",A," does not exist.")')
     statuscode=-1
     return
  endif else if ~fi.write then begin
     statusmsg=string(tgt_dirname,format='("'+routine_name+': Failed. Directory ",A," is not writeable by you.")')
     statuscode=-1
     return
  endif
  
  fi=file_info(filename)
  if (fi.directory) then begin
     statusmsg=string(filename,format='("'+routine_name+': Failed: ",A," is a directory.")')
     statuscode=-1
     return
  endif else if (fi.exists AND ~fi.write) then begin
     statusmsg=string(filename,format='("'+routine_name+': Failed. File ",A," exists, and is not writeable by you.")')
     statuscode=-1
     return
  end else if (fi.exists) then begin
   statusmsg=string(filename,format='("'+routine_name+': File ",A," already exists. Do you wish to overwrite it?")')
   answer=dialog_message(statusmsg,/question,/default_no, /center )
   if (answer NE 'Yes') then begin
      statusmsg=routine_name+': Save cancelled by user.'
      statuscode=-1
      return
   endif
 
  endif
endif else begin
  fi=file_info(filename)
  if (fi.directory) then begin
     statusmsg=string(filename,format='("'+routine_name+': Failed ",A," is a directory.")')
     statuscode=-1
     return
  endif else if (~fi.exists) then begin
     statusmsg=string(filename,format='("'+routine_name+': Failed. File ",A," does not exist.")')
     statuscode=-1
     return
  endif
endelse

end
