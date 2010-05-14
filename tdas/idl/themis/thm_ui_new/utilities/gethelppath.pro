;+
;
;NAME: gethelppath
;
;PURPOSE:
;  gets the path of the help directory in a cross platform way
;
;CALLING SEQUENCE:
;   gethelppath,path
;
;OUTPUT:
;   path:  the path to the resource directory
;
;
;$LastChangedBy: pcruce $
;$LastChangedDate: 2009-05-29 09:27:40 -0700 (Fri, 29 May 2009) $
;$LastChangedRevision: 5990 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/gethelppath.pro $
;----------

pro gethelppath,path

  ;get path of routine
 rt_info = routine_info('gethelppath',/source)
 path = file_dirname(rt_info.path) + '/../help/'

end