;+
;
;NAME: getresourcepath
;
;PURPOSE:
;  gets the path of the resource directory in a cross platform way
;
;CALLING SEQUENCE:
;   getresourcepath,path
;
;OUTPUT:
;   path:  the path to the resource directory
;
;
;$LastChangedBy: cgoethel_new $
;$LastChangedDate: 2009-03-12 14:23:23 -0700 (Thu, 12 Mar 2009) $
;$LastChangedRevision: 5268 $
;$URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/thmsoc/tags/tdas_5_21/idl/themis/thm_ui_new/utilities/getresourcepath.pro $
;----------

pro getresourcepath,path

  ;get path of routine
 rt_info = routine_info('getresourcepath',/source)
 path = file_dirname(rt_info.path) + '/../Resources/'

end